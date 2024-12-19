(defpackage #:ningle-fbr-test/package-mapper
  (:use #:cl
        #:rove)
  (:import-from #:ningle-fbr/package-mapper
                #:path->package))
(in-package :ningle-fbr-test/package-mapper)

(deftest package-mapper-test
  (testing "normal case"
    (ok (eq (path->package "/foo"
                           :system "app"
                           :routes-dir "routes")
            :app/routes/foo))
    (ok (eq (path->package "/foo"
                           :system "app"
                           :routes-dir "somedir/routes")
            :app/somedir/routes/foo)))

  (testing "with system-pathname"
    (ok (eq (path->package "/foo"
                           :system "app"
                           :system-path "src"
                           :routes-dir "src/routes")
            :app/routes/foo))
    (ok (eq (path->package "/foo"
                           :system "app"
                           :system-path "src"
                           :routes-dir "src/somedir/routes")
            :app/somedir/routes/foo))))
