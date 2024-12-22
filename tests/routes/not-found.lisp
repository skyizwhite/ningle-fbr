(defpackage #:ningle-fbr-test/routes/not-found
  (:use #:cl)
  (:import-from #:lack/response)
  (:import-from #:ningle)
  (:export #:handle-not-found))
(in-package #:ningle-fbr-test/routes/not-found)

(defun handle-not-found ()
  (setf (lack/response:response-status ningle:*response*)
        404)
  "Not Found")
