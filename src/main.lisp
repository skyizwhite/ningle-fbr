(uiop:define-package :ningle-fbr
  (:nicknames #:ningle-fbr/main)
  (:use #:cl)
  (:local-nicknames (#:re #:cl-ppcre))
  (:local-nicknames (#:ng #:ningle))
  (:export #:assign-routes))
(in-package :ningle-fbr)

(defun remove-file-type (namestr)
  (re:regex-replace ".lisp" namestr ""))

(defun remove-index (url)
  (if (string= url "/index")
      "/"
      (re:regex-replace "/index" url "")))

(defun replace-dynamic-annotation (url)
  (re:regex-replace "=" url ":"))

(defun format-url (url)
  (replace-dynamic-annotation (remove-index url)))

(defun pathname->url (pathname dir-namestring)
  (format-url
   (re:regex-replace dir-namestring
                     (remove-file-type (namestring pathname))
                     "")))

(defun pathname->package (pathname system-path-namestring system-prefix)
  (string-upcase
   (re:regex-replace system-path-namestring
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
            (dir->pathnames dir))))

(defparameter *http-request-methods*
  '(:GET :POST :PUT :DELETE :HEAD :CONNECT :OPTIONS :PATCH))

(defun assign-routes (app &key directory system)
  (loop
    :for (url . pkg) :in (dir->urls-and-packages directory system)
    :do (ql:quickload pkg) 
        (loop
          :for method :in *http-request-methods*
          :do (let ((handler (find-symbol (concatenate 'string "ON-" (string method))
                                          pkg)))
                (when handler
                  (setf (ng:route app url :method method) handler))))))
