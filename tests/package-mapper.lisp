(defpackage #:ningle-fbr-test/package-mapper
  (:use #:cl
        #:rove)
  (:import-from #:ningle-fbr/package-mapper
                #:path->package))
(in-package :ningle-fbr-test/package-mapper)

(deftest package-mapper-test
  (testing "normal case"
    (ok (string= (path->package "/foo"
                                :system "app"
                                :routes-dir "routes")
                 "APP/ROUTES/FOO"))
    (ok (string= (path->package "/foo"
                                :system "app"
                                :routes-dir "somedir/routes")
                 "APP/SOMEDIR/ROUTES/FOO")))

  (testing "with system-pathname"
    (ok (string= (path->package "/foo"
                                :system "app"
                                :system-path "src"
                                :routes-dir "src/routes")
                 "APP/ROUTES/FOO"))
    (ok (string= (path->package "/foo"
                                :system "app"
                                :system-path "src"
                                :routes-dir "src/somedir/routes")
                 "APP/SOMEDIR/ROUTES/FOO"))))
