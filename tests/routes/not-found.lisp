(defpackage #:ningle-fbr-test/routes/not-found
  (:use #:cl)
  (:export #:handle-not-found))
(in-package #:ningle-fbr-test/routes/not-found)

(defun handle-not-found ()
  "Not Found")
