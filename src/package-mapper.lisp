(defpackage #:ningle-fbr/package-mapper
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:split
                #:regex-replace)
  (:export #:path->package))
(in-package #:ningle-fbr/package-mapper)

(defun normalize-path (path)
  (remove-if #'(lambda (str) (string= str ""))
             (split "/" path)))

(defun remove-system-path (routes-dir system-path)
  (if system-path
      (regex-replace (format nil "^~a/?" (cl-ppcre:quote-meta-chars system-path))
                     routes-dir
                     "")
      routes-dir))

(defun path->package (path &key system (system-path nil) routes-dir)
  "Convert PATH to a package name based on given SYSTEM, SYSTEM-PATH, and ROUTES-DIR."
  (let* ((cleaned-routes-dir (remove-system-path routes-dir system-path))
         (routes-parts (normalize-path cleaned-routes-dir))
         (path-parts (normalize-path path))
         (all-parts (append (list system) routes-parts path-parts)))
    (string-upcase (format nil "~{~a~^/~}" all-parts))))
