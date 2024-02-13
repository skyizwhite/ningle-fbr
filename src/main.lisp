(uiop:define-package :ningle-fbr
  (:nicknames #:ningle-fbr/main)
  (:use #:cl
        #:ningle)
  (:export #:enable-file-based-routing))
(in-package :ningle-fbr)

(defun enable-file-based-routing (app directory)
  (declare (ignore app directory)))
