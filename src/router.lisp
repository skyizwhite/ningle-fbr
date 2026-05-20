(defpackage #:ningle-fbr/router
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:cl-ppcre
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
  "Translate an absolute file PATHNAME located under DIR-PATHNAME into a
leading-slash URL-like path. e.g. /…/routes/users/<id>.lisp under /…/routes/
becomes \"/users/<id>\"."
  (let* ((file-dirs (rest (pathname-directory pathname)))
         (base-dirs (rest (pathname-directory dir-pathname)))
         (relative-dirs (subseq file-dirs (length base-dirs))))
    (format nil "/~{~A/~}~A" relative-dirs (pathname-name pathname))))

(defun dynamic-path-p (path)
  (and (search "<" path) t))

(defun path-precedence< (a b)
  "Static paths sort before dynamic ones; ties broken lexicographically.
myway dispatches in registration order, so static routes must be installed
before parametric ones at the same depth."
  (let ((a-dyn (dynamic-path-p a))
        (b-dyn (dynamic-path-p b)))
    (cond
      ((and a-dyn (not b-dyn)) nil)
      ((and b-dyn (not a-dyn)) t)
      (t (string< a b)))))

(defun detect-paths (system dir)
  (let* ((dir-pathname
           (merge-pathnames (concatenate 'string dir "/")
                            (asdf:component-pathname (asdf:find-system system))))
         (paths (mapcar (lambda (pathname)
                          (pathname->path pathname dir-pathname))
                        (directory (merge-pathnames "**/*.lisp" dir-pathname)))))
    (sort paths #'path-precedence<)))

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
  '(:GET :POST :PUT :DELETE :HEAD :CONNECT :OPTIONS :PATCH :TRACE))

(defun install-not-found-handler (app pkg)
  (let ((handler (find-symbol "@NOT-FOUND" pkg)))
    (unless handler
      (error "Package ~A is mapped to /not-found but defines no @NOT-FOUND ~
              symbol. Did you forget to (:export #:@not-found)?" pkg))
    (defmethod ningle:not-found ((current-app (eql app)))
      (declare (ignore current-app))
      (setf (response-status ningle:*response*) 404)
      (funcall handler))))

(defun install-method-handlers (app uri pkg)
  (let ((registered 0))
    (dolist (method *http-request-methods*)
      (let ((handler (find-symbol (concatenate 'string "@" (string method))
                                  pkg)))
        (when handler
          (setf (ningle:route app uri :method method) handler)
          (incf registered))))
    (when (zerop registered)
      (warn "Route package ~A (URI ~A) exports no handler. ~
             Expected at least one of @GET, @POST, @PUT, @DELETE, …"
            pkg uri))))

(defmethod set-routes ((app ningle:app) &key system dir)
  (loop
    :for path :in (detect-paths system dir)
    :for uri := (path->uri path)
    :for pkg := (path->package path system dir)
    :do (load-system pkg)
        (if (string= uri "/not-found")
            (install-not-found-handler app pkg)
            (install-method-handlers app uri pkg))))
