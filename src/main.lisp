(uiop:define-package :ningle-fbr
  (:nicknames #:ningle-fbr/main)
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:import-from #:ningle)
  (:export #:assign-routes))
(in-package :ningle-fbr)

(defun remove-file-type (namestr)
  (cl-ppcre:regex-replace ".lisp" namestr ""))

(defun remove-index (url)
  (if (string= url "/index")
      "/"
      (cl-ppcre:regex-replace "/index" url "")))

(defun replace-dynamic-annotation (url)
  (cl-ppcre:regex-replace "=" url ":"))

(defun format-url (url)
  (replace-dynamic-annotation (remove-index url)))

(defun pathname->url (pathname dir-namestring)
  (format-url
   (cl-ppcre:regex-replace dir-namestring
                           (remove-file-type (namestring pathname))
                           "")))

(defun pathname->package (pathname system-path-namestring system-prefix)
  (string-upcase
   (cl-ppcre:regex-replace system-path-namestring
                           (remove-file-type (namestring pathname))
                           system-prefix)))

(defun dir->pathnames (dir)
  (directory (concatenate 'string
                          dir
                          "/**/*.lisp")))

(defun dir->urls-and-packages (dir system)
  (let ((dir-namestring (namestring
                         (asdf:system-relative-pathname system dir)))
        (system-path-namestring (namestring 
                                 (asdf/component:component-relative-pathname
                                  (asdf/find-system:find-system system))))
        (system-prefix (concatenate 'string system "/")))
    (mapcar (lambda (pathname)
              (cons (pathname->url pathname dir-namestring)
                    (pathname->package pathname system-path-namestring system-prefix)))
            (dir->pathnames dir-namestring))))

(defparameter *http-request-methods*
  '(:GET :POST :PUT :DELETE :HEAD :CONNECT :OPTIONS :PATCH))

(defun assign-routes (app &key directory system)
  (loop
    :for (url . pkg) :in (dir->urls-and-packages directory system)
    :do (ql:quickload pkg)
        (if (string= url "/not-found")
            (let ((handler (find-symbol "HANDLE-NOT-FOUND" pkg)))
              (defmethod ningle:not-found ((app ningle:app))
                (funcall handler))))
        (loop
          :for method :in *http-request-methods*
          :do (let ((handler (find-symbol (concatenate 'string "HANDLE-" (string method))
                                          pkg)))
                (when handler
                  (setf (ningle:route app url :method method) handler))))))
