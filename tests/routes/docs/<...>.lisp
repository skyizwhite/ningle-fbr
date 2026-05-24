(defpackage #:ningle-fbr-test/routes/docs/<...>
  (:use #:cl)
  (:export #:@get))
(in-package #:ningle-fbr-test/routes/docs/<...>)

(defun @get (params)
  (let ((splat (cdr (assoc :splat params))))
    (format nil "splat=~A" (first splat))))
