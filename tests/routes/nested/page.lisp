(defpackage #:ningle-fbr-test/routes/nested/page
  (:use #:cl)
  (:export #:handle-get))
(in-package #:ningle-fbr-test/routes/nested/page)

(defun handle-get (params)
  (declare (ignore params))
  "ok")
