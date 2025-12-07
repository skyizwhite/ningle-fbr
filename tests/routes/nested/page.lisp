(defpackage #:ningle-fbr-test/routes/nested/page
  (:use #:cl)
  (:export #:@get))
(in-package #:ningle-fbr-test/routes/nested/page)

(defun @get (params)
  (declare (ignore params))
  "ok")
