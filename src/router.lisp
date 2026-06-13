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
           #:path->package
           #:path-kind
           #:set-routes
           #:list-routes
           #:route-definition-error
           #:route-conflict-error
           #:route-conflict-error-conflicts
           #:missing-not-found-handler
           #:missing-not-found-handler-package
           #:missing-method-handlers
           #:missing-method-handlers-package
           #:missing-method-handlers-uri
           #:unbound-route-handler
           #:unbound-route-handler-package
           #:unbound-route-handler-uri
           #:unbound-route-handler-handler))
(in-package #:ningle-fbr/router)

(defun pathname->path (pathname dir-pathname)
  (let* ((file-directory (rest (pathname-directory pathname)))
         (base-directory (rest (pathname-directory dir-pathname)))
         (relative-directory (subseq file-directory (length base-directory))))
    (format nil "/~{~A/~}~A" relative-directory (pathname-name pathname))))

(defun catch-all-path-p (path)
  (and (search "<..." path) t))

(defun dynamic-path-p (path)
  (and (search "<" path) t))

(defun path-precedence-key (path)
  (cond ((catch-all-path-p path) 2)
        ((dynamic-path-p path) 1)
        (t 0)))

(defun path-precedence< (a b)
  (let ((key-a (path-precedence-key a))
        (key-b (path-precedence-key b)))
    (cond ((< key-a key-b) t)
          ((> key-a key-b) nil)
          (t (string< a b)))))

(defun discover-route-paths (system dir)
  (let* ((dir-pathname
           (merge-pathnames (concatenate 'string dir "/")
                            (asdf:component-pathname (asdf:find-system system))))
         (paths (mapcar (lambda (pathname)
                          (pathname->path pathname dir-pathname))
                        (directory (merge-pathnames "**/*.lisp" dir-pathname)))))
    (sort paths #'path-precedence<)))

(defun strip-index-segment (path)
  (if (string= path "/index")
      "/"
      (regex-replace "/index$" path "")))

(defun bracket->myway (path)
  (let ((after-catch-all (regex-replace-all "<\\.\\.\\.[^>]*>" path "*")))
    (regex-replace-all "<([^>]+)>" after-catch-all ":\\1")))

(defun path->uri (path)
  (bracket->myway (strip-index-segment path)))

(defun path-kind (path uri)
  (cond ((string= uri "/not-found") :not-found)
        ((catch-all-path-p path) :catch-all)
        ((dynamic-path-p path) :dynamic)
        (t :static)))

(defun path->package (path system dir)
  (make-keyword (string-upcase (concatenate 'string
                                            (string system)
                                            "/"
                                            dir
                                            path))))

(defparameter *http-request-methods*
  '(:GET :POST :PUT :DELETE :HEAD :CONNECT :OPTIONS :PATCH :TRACE))

(defparameter *http-method-handler-names*
  (mapcar (lambda (method)
            (cons method (concatenate 'string "@" (string method))))
          *http-request-methods*))

(defun ensure-route-package-loaded (pkg)
  (unless (find-package pkg)
    (load-system pkg)))

(defun find-exported-symbol (name pkg)
  (multiple-value-bind (symbol status) (find-symbol name pkg)
    (and (eq status :external) symbol)))

(defun exported-http-methods (pkg)
  (loop for (method . handler-name) in *http-method-handler-names*
        when (find-exported-symbol handler-name pkg)
          collect method))

(define-condition route-definition-error (error) ()
  (:documentation "Base condition for route loading and installation problems."))

(define-condition route-conflict-error (route-definition-error)
  ((conflicts :initarg :conflicts
              :reader route-conflict-error-conflicts))
  (:report
   (lambda (condition stream)
     (format stream
             "ningle-fbr: multiple route files map to the same URI:~%~{  ~A~%~}"
             (loop for (uri . paths) in (route-conflict-error-conflicts condition)
                   collect (format nil "~S <- ~{~S~^, ~}" uri paths)))))
  (:documentation "Signalled when several route files map to the same URI."))

(define-condition missing-not-found-handler (route-definition-error)
  ((package :initarg :package
            :reader missing-not-found-handler-package))
  (:report
   (lambda (condition stream)
     (format stream
             "Route package ~A is mapped to /not-found but does not export an ~
              @NOT-FOUND symbol. Did you forget to (:export #:@not-found)?"
             (missing-not-found-handler-package condition))))
  (:documentation "Signalled when a /not-found route package does not export @NOT-FOUND."))

(define-condition missing-method-handlers (route-definition-error)
  ((package :initarg :package
            :reader missing-method-handlers-package)
   (uri :initarg :uri
        :reader missing-method-handlers-uri))
  (:report
   (lambda (condition stream)
     (format stream
             "Route package ~A (URI ~A) exports no handler. ~
              Expected at least one of @GET, @POST, @PUT, @DELETE, …"
             (missing-method-handlers-package condition)
             (missing-method-handlers-uri condition))))
  (:documentation "Signalled when a route package exports no @METHOD handler."))

(define-condition unbound-route-handler (route-definition-error)
  ((package :initarg :package
            :reader unbound-route-handler-package)
   (uri :initarg :uri
        :initform nil
        :reader unbound-route-handler-uri)
   (handler :initarg :handler
            :reader unbound-route-handler-handler))
  (:report
   (lambda (condition stream)
     (let ((uri (unbound-route-handler-uri condition)))
       (if uri
           (format stream
                   "Route package ~A (URI ~A) exports ~A but it has no ~
                    function definition."
                   (unbound-route-handler-package condition)
                   uri
                   (unbound-route-handler-handler condition))
           (format stream
                   "Route package ~A exports ~A but it has no function ~
                    definition."
                   (unbound-route-handler-package condition)
                   (unbound-route-handler-handler condition))))))
  (:documentation "Signalled when an exported handler symbol has no function definition."))

(defun uri-match-key (uri)
  (regex-replace-all ":[^/]+"
                     (regex-replace-all "\\*" uri "<<SPLAT>>")
                     "<<PARAM>>"))

(defun check-route-conflicts (paths)
  (let ((paths-by-match-key (make-hash-table :test 'equal)))
    (dolist (path paths)
      (let ((uri (path->uri path)))
        (push (cons path uri)
              (gethash (uri-match-key uri) paths-by-match-key))))
    (let ((conflicts
            (loop for entries being the hash-values of paths-by-match-key
                  when (cdr entries)
                    collect (let ((sorted-entries (sort (copy-list entries)
                                                        #'string<
                                                        :key #'car)))
                              (cons (cdr (first sorted-entries))
                                    (mapcar #'car sorted-entries))))))
      (when conflicts
        (error 'route-conflict-error
               :conflicts (sort conflicts #'string< :key #'car))))))

(defun install-not-found-handler (app pkg)
  (let ((handler (find-exported-symbol "@NOT-FOUND" pkg)))
    (unless handler
      (error 'missing-not-found-handler :package pkg))
    (unless (fboundp handler)
      (error 'unbound-route-handler :package pkg :handler handler))
    (defmethod ningle:not-found ((current-app (eql app)))
      (declare (ignore current-app))
      (setf (response-status ningle:*response*) 404)
      (funcall handler))))

(defun install-method-handlers (app uri pkg methods)
  (when (null methods)
    (error 'missing-method-handlers :package pkg :uri uri))
  (dolist (method methods)
    (let ((handler (find-exported-symbol
                    (cdr (assoc method *http-method-handler-names*))
                    pkg)))
      (unless (fboundp handler)
        (error 'unbound-route-handler
               :package pkg :uri uri :handler handler))
      (setf (ningle:route app uri :method method) handler))))

(defun list-routes (&key system dir)
  "Return a plist for every route file under DIR (relative to ASDF SYSTEM's
source root). Each plist has :PATH, :URI, :PACKAGE, :KIND (:STATIC, :DYNAMIC,
:CATCH-ALL, or :NOT-FOUND), and :METHODS (the exported HTTP method keywords)."
  (let ((paths (discover-route-paths system dir)))
    (check-route-conflicts paths)
    (loop for path in paths
          for uri = (path->uri path)
          for pkg = (path->package path system dir)
          do (ensure-route-package-loaded pkg)
          collect (list :path path
                        :uri uri
                        :package pkg
                        :kind (path-kind path uri)
                        :methods (if (string= uri "/not-found")
                                     (when (find-exported-symbol "@NOT-FOUND" pkg)
                                       '(:not-found))
                                     (exported-http-methods pkg))))))

(defmethod set-routes ((app ningle:app) &key system dir)
  "Install every route file under DIR (relative to ASDF SYSTEM's source root)
onto APP. The package mapped to /not-found becomes APP's custom 404 handler.
Signals a ROUTE-DEFINITION-ERROR on conflicts, missing handlers, or unbound
handler symbols."
  (dolist (route (list-routes :system system :dir dir))
    (let ((pkg (getf route :package)))
      (if (eq (getf route :kind) :not-found)
          (install-not-found-handler app pkg)
          (install-method-handlers app
                                   (getf route :uri)
                                   pkg
                                   (getf route :methods))))))
