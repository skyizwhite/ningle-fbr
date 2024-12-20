(defpackage #:ningle-fbr-test/router
  (:use #:cl
        #:rove)
  (:import-from #:ningle-fbr/router
                #:pathname->path
                #:path->uri
                #:path->package))
(in-package #:ningle-fbr-test/router)

(deftest router-test
  (testing "pathname->path"
    (ok (string= (pathname->path #P"/home/app/src/routes/foo.lisp"
                                 #P"/home/app/src/routes/")
                 "/foo"))))

(deftest uri-test
  (testing "normal path"
    (ok (string= (path->uri "/foo") "/foo"))
    (ok (string= (path->uri "/foo/bar") "/foo/bar")))
  
  (testing "index path"
    (ok (string= (path->uri "/index") "/"))
    (ok (string= (path->uri "/nested/index") "/nested")))
  
  (testing "dynamic path"
    (ok (string= (path->uri "/user/[id]") "/user/:id"))
    (ok (string= (path->uri "/location/[country]/[city]") "/location/:country/:city" ))))

(deftest package-test
  (testing "normal case"
    (ok (eq (path->package "/foo" :app "routes")
            :app/routes/foo))
    (ok (eq (path->package "/foo" :app "somedir/routes")
            :app/somedir/routes/foo))))
