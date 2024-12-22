(defpackage #:ningle-fbr-test/routes/users/<id>
  (:use #:cl)
  (:export #:handle-get))
(in-package #:ningle-fbr-test/routes/users/<id>)

(defun handle-get (params)
  (let ((id (cdr (assoc :id params))))
    id))
