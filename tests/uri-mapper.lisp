(defpackage #:ningle-fbr-test/uri-mapper
  (:use #:cl
        #:rove)
  (:import-from #:ningle-fbr/uri-mapper
                #:path->uri))
(in-package #:ningle-fbr-test/uri-mapper)

(deftest uri-mapper-test
  (testing "normal path"
    (ok (string= (path->uri "/foo") "/foo"))
    (ok (string= (path->uri "/foo/bar") "/foo/bar")))
  
  (testing "index path"
    (ok (string= (path->uri "/index") "/"))
    (ok (string= (path->uri "/nested/index") "/nested")))
  
  (testing "dynamic path"
    (ok (string= (path->uri "/user/[id]") "/user/:id"))
    (ok (string= (path->uri "/location/[country]/[city]") "/location/:country/:city" ))))
