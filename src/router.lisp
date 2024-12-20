(defpackage #:ningle-fbr/router
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:cl-ppcre
                #:quote-meta-chars
                #:regex-replace
                #:regex-replace-all)
  (:import-from #:ningle)
  (:export #:pathname->path
           #:path->uri
           #:path-package
           #:set-routes))
(in-package #:ningle-fbr/router)

(defun pathname->path (pathname &key target-pathname)
  (let* ((full (namestring pathname))
         (prefix (quote-meta-chars (namestring target-pathname))))
    (regex-replace (format nil "^~A(.*?).lisp$" prefix) full "/\\1")))

(defun detect-paths (system target-path)
  (let ((target-pathname (asdf:system-relative-pathname system
                                                        (concatenate 'string
                                                                     target-path
                                                                     "/"))))
    (mapcar (lambda (pathname)
              (pathname->path pathname :target-pathname target-pathname))
            (directory (merge-pathnames "**/*.lisp" target-pathname)))))

(defun remove-index (path)
  (if (string= path "/index")
      "/"
      (regex-replace "/index$" path "")))

(defun bracket->colon (path)
  (regex-replace-all "\\[(.*?)\\]" path ":\\1"))

(defun path->uri (path)
  (bracket->colon (remove-index path)))

(defun path->package (path &key system target-path)
  (make-keyword (string-upcase (concatenate 'string
                                            system
                                            "/"
                                            target-path
                                            path))))

(defparameter *http-request-methods*
  '(:GET :POST :PUT :DELETE :HEAD :CONNECT :OPTIONS :PATCH))

(defmethod set-routes ((app ningle:app) &key system target-path)
  (loop
    :for path :in (detect-paths system target-path)
    :with uri := (path->uri path)
    :with pkg := (path->package path
                                :system system
                                :target-path target-path)
    :do (asdf:compile-system pkg)
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
