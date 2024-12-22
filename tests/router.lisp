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
                               :target-dir-path "routes")
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
        (ok (string= body "Not Found"))
        (ok (eql status 404))))))

