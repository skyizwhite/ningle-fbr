(defpackage #:ningle-fbr-test/routes/not-found
  (:use #:cl)
  (:export #:@not-found))
(in-package #:ningle-fbr-test/routes/not-found)

(defun @not-found ()
  "Not Found")
