(defpackage #:ningle-fbr-test/routes/hello
  (:use #:cl)
  (:export #:handle-get))
(in-package #:ningle-fbr-test/routes/hello)

(defun handle-get (params)
  (declare (ignore params))
  "ok")
