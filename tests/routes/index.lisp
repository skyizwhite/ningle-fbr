(defpackage #:ningle-fbr-test/routes/index
  (:use #:cl)
  (:export #:handle-get))
(in-package #:ningle-fbr-test/routes/index)

(defun handle-get (params)
  (declare (ignore params))
  "ok")
