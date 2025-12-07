(defpackage #:ningle-fbr/router
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:cl-ppcre
                #:quote-meta-chars
                #:regex-replace
                #:regex-replace-all)
  (:import-from #:ningle)
  (:import-from #:lack/response
                #:response-status)
  (:import-from #:trivial-system-loader
                #:load-system)
  (:export #:pathname->path
           #:path->uri
           #:path-package
           #:set-routes))
(in-package #:ningle-fbr/router)

(defun pathname->path (pathname dir-pathname)
  (let* ((full (namestring pathname))
         (prefix (quote-meta-chars (namestring dir-pathname))))
    (regex-replace (format nil "^~A(.*?).lisp$" prefix) full "/\\1")))

(defun detect-paths (system dir)
  (let ((dir-pathname
          (merge-pathnames (concatenate 'string dir "/")
                           (asdf:component-pathname (asdf:find-system system)))))
    (mapcar (lambda (pathname)
              (pathname->path pathname dir-pathname))
            (directory (merge-pathnames "**/*.lisp" dir-pathname)))))

(defun remove-index (path)
  (if (string= path "/index")
      "/"
      (regex-replace "/index$" path "")))

(defun bracket->colon (path)
  (regex-replace-all "<(.*?)>" path ":\\1"))

(defun path->uri (path)
  (bracket->colon (remove-index path)))

(defun path->package (path system dir)
  (make-keyword (string-upcase (concatenate 'string
                                            (string system)
                                            "/"
                                            dir
                                            path))))

(defparameter *http-request-methods*
  '(:GET :POST :PUT :DELETE :HEAD :CONNECT :OPTIONS :PATCH))

(defmethod set-routes ((app ningle:app) &key system dir)
  (loop
    :for path :in (detect-paths system dir)
    :for uri := (path->uri path)
    :for pkg := (path->package path system dir)
    :do (load-system pkg)
        (if (string= uri "/not-found")
            (let ((handler (find-symbol "HANDLE-NOT-FOUND" pkg)))
              (defmethod ningle:not-found ((app (eql app)))
                (setf (response-status ningle:*response*) 404)
                (funcall handler))))
        (loop
          :for method :in *http-request-methods*
          :do (let ((handler (find-symbol (concatenate 'string "HANDLE-" (string method))
                                          pkg)))
                (when handler
                  (setf (ningle:route app uri :method method) handler))))))
