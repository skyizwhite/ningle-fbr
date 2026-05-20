(defpackage #:ningle-fbr-test/router
  (:use #:cl
        #:rove)
  (:import-from #:ningle)
  (:import-from #:lack)
  (:import-from #:lack/test
                #:testing-app
                #:request)
  (:import-from #:ningle-fbr/router
                #:path->uri
                #:path->package
                #:pathname->path
                #:set-routes))
(in-package #:ningle-fbr-test/router)

(deftest uri-test
  (testing "normal path"
    (ok (string= (path->uri "/foo") "/foo"))
    (ok (string= (path->uri "/foo/bar") "/foo/bar")))
  
  (testing "index path"
    (ok (string= (path->uri "/index") "/"))
    (ok (string= (path->uri "/nested/index") "/nested")))
  
  (testing "dynamic path"
    (ok (string= (path->uri "/user/<id>") "/user/:id"))
    (ok (string= (path->uri "/location/<country>/<city>") "/location/:country/:city"))))

(deftest package-test
  (testing "normal case"
    (ok (eq (path->package "/foo" :app "routes")
            :app/routes/foo))
    (ok (eq (path->package "/foo" :app "somedir/routes")
            :app/somedir/routes/foo))))

(deftest router-test
  (testing "pathname->path"
    (ok (string= (pathname->path #P"/home/app/src/routes/foo.lisp"
                                 #P"/home/app/src/routes/")
                 "/foo")))
  
  (testing "set-routes"
    (testing-app (let ((app (make-instance 'ningle:app)))
                   (set-routes app
                               :system :ningle-fbr-test
                               :dir "routes")
                   (lack:builder app))
      (multiple-value-bind (body status headers)
          (request "/")
        (declare (ignore headers))
        (ok (string= body "ok"))
        (ok (eql status 200)))

      (multiple-value-bind (body status headers)
          (request "/hello")
        (declare (ignore headers))
        (ok (string= body "ok"))
        (ok (eql status 200)))
      
      (multiple-value-bind (body status headers)
          (request "/nested/page")
        (declare (ignore headers))
        (ok (string= body "ok"))
        (ok (eql status 200)))
      
      (multiple-value-bind (body status headers)
          (request "/users")
        (declare (ignore headers))
        (ok (string= body "ok"))
        (ok (eql status 200)))
      
      (multiple-value-bind (body status headers)
          (request "/users/bob")
        (declare (ignore headers))
        (ok (string= body "bob"))
        (ok (eql status 200)))
      
      (multiple-value-bind (body status headers)
          (request "/missing")
        (declare (ignore headers))
        (ok (string= body "custom-not-found"))
        (ok (eql status 404))))))

(defmacro with-empty-package ((var) &body body)
  "Bind VAR to a freshly interned, empty package and delete it afterwards."
  `(let ((,var (make-package (gensym "FBR-TEST-EMPTY-PKG-") :use nil)))
     (unwind-protect (progn ,@body)
       (delete-package ,var))))

(deftest install-not-found-handler-test
  (testing "signals an error when @not-found is not exported from the package"
    (with-empty-package (pkg)
      (let ((app (make-instance 'ningle:app)))
        (ok (signals
                (ningle-fbr/router::install-not-found-handler app pkg)
                'error)))))

  (testing "signals an error when @not-found is interned but not exported"
    (with-empty-package (pkg)
      (let* ((app (make-instance 'ningle:app))
             (sym (intern "@NOT-FOUND" pkg)))
        (setf (symbol-function sym) (lambda () "nope"))
        (ok (signals
                (ningle-fbr/router::install-not-found-handler app pkg)
                'error)))))

  (testing "signals an error when @not-found is exported but has no function definition"
    (with-empty-package (pkg)
      (let* ((app (make-instance 'ningle:app))
             (sym (intern "@NOT-FOUND" pkg)))
        (export sym pkg)
        (ok (signals
                (ningle-fbr/router::install-not-found-handler app pkg)
                'error))))))

(deftest install-method-handlers-test
  (testing "signals an error when no @GET/@POST/... handler is exported"
    (with-empty-package (pkg)
      (let ((app (make-instance 'ningle:app)))
        (ok (signals
                (ningle-fbr/router::install-method-handlers app "/empty" pkg)
                'error)))))

  (testing "signals an error when a handler is interned but not exported"
    (with-empty-package (pkg)
      (let* ((app (make-instance 'ningle:app))
             (sym (intern "@GET" pkg)))
        (setf (symbol-function sym)
              (lambda (params) (declare (ignore params)) "ok"))
        (ok (signals
                (ningle-fbr/router::install-method-handlers app "/internal" pkg)
                'error)))))

  (testing "signals an error when a handler is exported but has no function definition"
    (with-empty-package (pkg)
      (let* ((app (make-instance 'ningle:app))
             (sym (intern "@GET" pkg)))
        (export sym pkg)
        (ok (signals
                (ningle-fbr/router::install-method-handlers app "/unbound" pkg)
                'error)))))

  (testing "does not signal when at least one handler is exported and fboundp"
    (with-empty-package (pkg)
      (let* ((app (make-instance 'ningle:app))
             (sym (intern "@GET" pkg)))
        (setf (symbol-function sym)
              (lambda (params) (declare (ignore params)) "ok"))
        (export sym pkg)
        (ok (handler-case
                (progn
                  (ningle-fbr/router::install-method-handlers app "/has-handler" pkg)
                  t)
              (error () nil)))))))

