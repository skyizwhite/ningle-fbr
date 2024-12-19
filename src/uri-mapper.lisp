(defpackage #:ningle-fbr/uri-mapper
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:export #:path->uri))
(in-package #:ningle-fbr/uri-mapper)

(defun remove-index (str)
  (if (string= str "/index")
      "/"
      (cl-ppcre:regex-replace "/index$" str "")))

(defun colon->bracket (str)
  (cl-ppcre:regex-replace-all "\\[(.*?)\\]" str ":\\1"))

(defun path->uri (path)
  (colon->bracket (remove-index path)))
