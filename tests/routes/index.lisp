(defpackage #:ningle-fbr-test/routes/index
  (:use #:cl)
  (:export #:@get))
(in-package #:ningle-fbr-test/routes/index)

(defun @get (params)
  (declare (ignore params))
  "ok")
