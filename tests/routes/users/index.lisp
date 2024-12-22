(defpackage #:ningle-fbr-test/routes/users/index
  (:use #:cl)
  (:export #:handle-get))
(in-package #:ningle-fbr-test/routes/users/index)

(defun handle-get (params)
  (declare (ignore params))
  "ok")
