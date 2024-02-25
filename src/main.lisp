(uiop:define-package :ningle-fbr
  (:nicknames #:ningle-fbr/main)
  (:use #:cl)
  (:local-nicknames (#:alx #:alexandria))
  (:local-nicknames (#:re #:cl-ppcre))
  (:local-nicknames (#:ng #:ningle))
  (:export #:enable-file-based-routing))
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

(defun pathname->url (pathname dir system)
  (format-url
   (re:regex-replace (namestring (asdf:system-relative-pathname system dir))
                     (remove-file-type (namestring pathname))
                     "")))

(defun pathname->package (pathname system system-pathname)
  (alx:make-keyword
   (string-upcase
    (re:regex-replace (namestring system-pathname)
                      (remove-file-type (namestring pathname))
                      (concatenate 'string system "/")))))

(defun dir->pathnames (dir)
  (directory (concatenate 'string
                          dir
                          "/**/*.lisp")))

(defun dir->urls (dir system)
  (mapcar (lambda (pathname)
            (pathname->url pathname dir system))
          (dir->pathnames dir)))

(defun dir->packages (dir system system-pathname)
  (mapcar (lambda (pathname)
            (pathname->package pathname system system-pathname))
          (dir->pathnames dir)))

(defparameter *http-request-methods*
  '(:GET :HEAD :POST :PUT :DELETE :CONNECT :OPTIONS :PATCH))

(defun enable-file-based-routing (app &key directory system)
  (let* ((system-pathname (asdf/component:component-relative-pathname
                           (asdf/find-system:find-system system)))
         (urls (dir->urls directory system))
         (packages (dir->packages directory system system-pathname)))
    (ql:quickload packages)
    (loop
      :for url :in urls
      :for pkg :in packages
      :do (loop
            :for method :in *http-request-methods*
            :do (let ((handler (find-symbol (string (alx:symbolicate 'on- method)) pkg)))
                  (when handler
                    (setf (ng:route app url :method method) handler)))))))
