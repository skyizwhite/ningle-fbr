(defpackage #:ningle-fbr-test/routes/users/index
  (:use #:cl)
  (:export #:@get))
(in-package #:ningle-fbr-test/routes/users/index)

(defun @get (params)
  (declare (ignore params))
  "ok")
