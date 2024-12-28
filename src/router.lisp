(defpackage #:ningle-fbr/router
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:cl-ppcre
                #:quote-meta-chars
                #:regex-replace
                #:regex-replace-all)
  (:import-from #:ningle)
  (:import-from #:trivial-system-loader
                #:load-system)
  (:export #:pathname->path
           #:path->uri
           #:path-package
           #:set-routes))
(in-package #:ningle-fbr/router)

(defun pathname->path (pathname target-dir-pathname)
  (let* ((full (namestring pathname))
         (prefix (quote-meta-chars (namestring target-dir-pathname))))
    (regex-replace (format nil "^~A(.*?).lisp$" prefix) full "/\\1")))

(defun detect-paths (system target-dir-path)
  (let ((target-dir-pathname
          (merge-pathnames (concatenate 'string
                                        target-dir-path
                                        "/")
                           (asdf:component-pathname (asdf:find-system system)))))
    (mapcar (lambda (pathname)
              (pathname->path pathname target-dir-pathname))
            (directory (merge-pathnames "**/*.lisp" target-dir-pathname)))))

(defun remove-index (path)
  (if (string= path "/index")
      "/"
      (regex-replace "/index$" path "")))

(defun bracket->colon (path)
  (regex-replace-all "<(.*?)>" path ":\\1"))

(defun path->uri (path)
  (bracket->colon (remove-index path)))

(defun path->package (path system target-dir-path)
  (make-keyword (string-upcase (concatenate 'string
                                            (string system)
                                            "/"
                                            target-dir-path
                                            path))))

(defparameter *http-request-methods*
  '(:GET :POST :PUT :DELETE :HEAD :CONNECT :OPTIONS :PATCH))

(defmethod set-routes ((app ningle:app) &key system target-dir-path)
  (loop
    :for path :in (detect-paths system target-dir-path)
    :for uri := (path->uri path)
    :for pkg := (path->package path system target-dir-path)
    :do (load-system pkg)
        (if (string= uri "/not-found")
            (let ((handler (find-symbol "HANDLE-NOT-FOUND" pkg)))
              (defmethod ningle:not-found ((app ningle:app))
                (funcall handler))))
        (loop
          :for method :in *http-request-methods*
          :do (let ((handler (find-symbol (concatenate 'string "HANDLE-" (string method))
                                          pkg)))
                (when handler
                  (setf (ningle:route app uri :method method) handler))))))
