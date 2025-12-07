(defpackage #:ningle-fbr-test/routes/users/<id>
  (:use #:cl)
  (:export #:@get))
(in-package #:ningle-fbr-test/routes/users/<id>)

(defun @get (params)
  (let ((id (cdr (assoc :id params))))
    id))
