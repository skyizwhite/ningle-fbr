(defpackage #:ningle-fbr-test/routes/hello
  (:use #:cl)
  (:export #:@get))
(in-package #:ningle-fbr-test/routes/hello)

(defun @get (params)
  (declare (ignore params))
  "ok")
