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
                #:path-kind
                #:pathname->path
                #:set-routes
                #:list-routes
                #:route-conflict-error
                #:route-conflict-error-conflicts
                #:missing-not-found-handler
                #:missing-method-handlers
                #:unbound-route-handler))
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
    (ok (string= (path->uri "/location/<country>/<city>") "/location/:country/:city")))

  (testing "catch-all path"
    (ok (string= (path->uri "/docs/<...>") "/docs/*"))
    (ok (string= (path->uri "/docs/<...slug>") "/docs/*"))
    (ok (string= (path->uri "/<...>") "/*"))
    (ok (string= (path->uri "/<id>/<...rest>") "/:id/*"))))

(deftest path-kind-test
  (ok (eq (path-kind "/index" "/") :static))
  (ok (eq (path-kind "/users/<id>" "/users/:id") :dynamic))
  (ok (eq (path-kind "/docs/<...>" "/docs/*") :catch-all))
  (ok (eq (path-kind "/not-found" "/not-found") :not-found)))

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
          (request "/docs/intro/getting-started")
        (declare (ignore headers))
        (ok (string= body "splat=intro/getting-started"))
        (ok (eql status 200)))

      (multiple-value-bind (body status headers)
          (request "/missing")
        (declare (ignore headers))
        (ok (string= body "custom-not-found"))
        (ok (eql status 404))))))

(deftest list-routes-test
  (let ((routes (list-routes :system :ningle-fbr-test :dir "routes")))
    (testing "every detected route is reported"
      (ok (find "/" routes :key (lambda (r) (getf r :uri)) :test #'string=))
      (ok (find "/hello" routes :key (lambda (r) (getf r :uri)) :test #'string=))
      (ok (find "/users" routes :key (lambda (r) (getf r :uri)) :test #'string=))
      (ok (find "/users/:id" routes :key (lambda (r) (getf r :uri)) :test #'string=))
      (ok (find "/docs/*" routes :key (lambda (r) (getf r :uri)) :test #'string=))
      (ok (find "/not-found" routes :key (lambda (r) (getf r :uri)) :test #'string=)))

    (testing "kinds are classified"
      (ok (eq (getf (find "/" routes :key (lambda (r) (getf r :uri)) :test #'string=) :kind)
              :static))
      (ok (eq (getf (find "/users/:id" routes :key (lambda (r) (getf r :uri)) :test #'string=) :kind)
              :dynamic))
      (ok (eq (getf (find "/docs/*" routes :key (lambda (r) (getf r :uri)) :test #'string=) :kind)
              :catch-all))
      (ok (eq (getf (find "/not-found" routes :key (lambda (r) (getf r :uri)) :test #'string=) :kind)
              :not-found)))

    (testing "methods are introspected"
      (ok (equal (getf (find "/" routes :key (lambda (r) (getf r :uri)) :test #'string=) :methods)
                 '(:GET)))
      (ok (equal (getf (find "/not-found" routes :key (lambda (r) (getf r :uri)) :test #'string=) :methods)
                 '(:not-found))))

    (testing "static routes are ordered before dynamic, dynamic before catch-all"
      (let ((kinds (mapcar (lambda (r) (getf r :kind)) routes)))
        (ok (every (lambda (k) (member k '(:static :dynamic :catch-all :not-found))) kinds))
        (let ((static-pos (position :static kinds :from-end t))
              (dynamic-pos (position :dynamic kinds))
              (catch-all-pos (position :catch-all kinds)))
          (when (and static-pos dynamic-pos)
            (ok (< static-pos dynamic-pos)))
          (when (and dynamic-pos catch-all-pos)
            (ok (< dynamic-pos catch-all-pos))))))))

(deftest check-route-conflicts-test
  (testing "duplicate URIs (e.g. <id> and <name>) raise a conflict error"
    (let ((err (handler-case
                   (ningle-fbr/router::check-route-conflicts
                    '("/users/<id>" "/users/<name>"))
                 (route-conflict-error (c) c))))
      (ok (typep err 'route-conflict-error))
      (let ((conflicts (route-conflict-error-conflicts err)))
        (ok (= 1 (length conflicts)))
        (ok (string= (caar conflicts) "/users/:id"))
        (ok (equal (cdar conflicts) '("/users/<id>" "/users/<name>"))))))

  (testing "an index file and a sibling file mapping to the same URI conflict"
    (ok (signals
            (ningle-fbr/router::check-route-conflicts
             '("/users/index" "/users"))
            'route-conflict-error)))

  (testing "non-conflicting paths pass through"
    (ok (null (ningle-fbr/router::check-route-conflicts
               '("/" "/hello" "/users" "/users/<id>" "/docs/<...>"))))))

(defmacro with-empty-package ((var) &body body)
  "Bind VAR to a freshly interned, empty package and delete it afterwards."
  `(let ((,var (make-package (gensym "FBR-TEST-EMPTY-PKG-") :use nil)))
     (unwind-protect (progn ,@body)
       (delete-package ,var))))

(deftest install-not-found-handler-test
  (testing "signals MISSING-NOT-FOUND-HANDLER when @not-found is not exported"
    (with-empty-package (pkg)
      (let ((app (make-instance 'ningle:app)))
        (ok (signals
                (ningle-fbr/router::install-not-found-handler app pkg)
                'missing-not-found-handler)))))

  (testing "signals MISSING-NOT-FOUND-HANDLER when @not-found is interned but not exported"
    (with-empty-package (pkg)
      (let* ((app (make-instance 'ningle:app))
             (sym (intern "@NOT-FOUND" pkg)))
        (setf (symbol-function sym) (lambda () "nope"))
        (ok (signals
                (ningle-fbr/router::install-not-found-handler app pkg)
                'missing-not-found-handler)))))

  (testing "signals UNBOUND-ROUTE-HANDLER when @not-found is exported but has no function definition"
    (with-empty-package (pkg)
      (let* ((app (make-instance 'ningle:app))
             (sym (intern "@NOT-FOUND" pkg)))
        (export sym pkg)
        (ok (signals
                (ningle-fbr/router::install-not-found-handler app pkg)
                'unbound-route-handler))))))

(deftest install-method-handlers-test
  (testing "signals MISSING-METHOD-HANDLERS when no @GET/@POST/... handler is exported"
    (with-empty-package (pkg)
      (let ((app (make-instance 'ningle:app)))
        (ok (signals
                (ningle-fbr/router::install-method-handlers app "/empty" pkg)
                'missing-method-handlers)))))

  (testing "signals MISSING-METHOD-HANDLERS when a handler is interned but not exported"
    (with-empty-package (pkg)
      (let* ((app (make-instance 'ningle:app))
             (sym (intern "@GET" pkg)))
        (setf (symbol-function sym)
              (lambda (params) (declare (ignore params)) "ok"))
        (ok (signals
                (ningle-fbr/router::install-method-handlers app "/internal" pkg)
                'missing-method-handlers)))))

  (testing "signals UNBOUND-ROUTE-HANDLER when a handler is exported but has no function definition"
    (with-empty-package (pkg)
      (let* ((app (make-instance 'ningle:app))
             (sym (intern "@GET" pkg)))
        (export sym pkg)
        (ok (signals
                (ningle-fbr/router::install-method-handlers app "/unbound" pkg)
                'unbound-route-handler)))))

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

