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
           #:route-conflict-error
           #:route-conflict-error-conflicts))
(in-package #:ningle-fbr/router)

(defun pathname->path (pathname dir-pathname)
  "Translate an absolute file PATHNAME located under DIR-PATHNAME into a
leading-slash URL-like path. e.g. /…/routes/users/<id>.lisp under /…/routes/
becomes \"/users/<id>\"."
  (let* ((file-dirs (rest (pathname-directory pathname)))
         (base-dirs (rest (pathname-directory dir-pathname)))
         (relative-dirs (subseq file-dirs (length base-dirs))))
    (format nil "/~{~A/~}~A" relative-dirs (pathname-name pathname))))

(defun catch-all-path-p (path)
  (and (search "<..." path) t))

(defun dynamic-path-p (path)
  (and (search "<" path) t))

(defun path-precedence-key (path)
  "Static (0) < dynamic (1) < catch-all (2). myway dispatches in registration
order, so more-specific routes must be installed first."
  (cond ((catch-all-path-p path) 2)
        ((dynamic-path-p path) 1)
        (t 0)))

(defun path-precedence< (a b)
  "Order by precedence key, ties broken lexicographically."
  (let ((ka (path-precedence-key a))
        (kb (path-precedence-key b)))
    (cond ((< ka kb) t)
          ((> ka kb) nil)
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

(defun bracket->myway (path)
  "Translate file-based parameter syntax into myway's URL grammar:
  <...> or <...name> → *      (catch-all, surfaced as :SPLAT in params)
  <name>             → :name  (named parameter)
The catch-all transformation runs first so that <...rest> is not mis-parsed
as the named parameter :...rest."
  (let ((step1 (regex-replace-all "<\\.\\.\\.[^>]*>" path "*")))
    (regex-replace-all "<([^>]+)>" step1 ":\\1")))

(defun path->uri (path)
  (bracket->myway (remove-index path)))

(defun path-kind (path uri)
  "Classify a detected route file. URI is needed to recognise /not-found."
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

(defun find-exported-symbol (name pkg)
  (multiple-value-bind (symbol status) (find-symbol name pkg)
    (and (eq status :external) symbol)))

(defun exported-methods (pkg)
  "List the HTTP method keywords this package exports a handler for."
  (loop for method in *http-request-methods*
        when (find-exported-symbol (concatenate 'string "@" (string method)) pkg)
          collect method))

(define-condition route-conflict-error (error)
  ((conflicts :initarg :conflicts
              :reader route-conflict-error-conflicts
              :documentation "Alist of (URI . PATHS) where PATHS is a list of
file-derived paths colliding on URI."))
  (:report
   (lambda (condition stream)
     (format stream
             "ningle-fbr: multiple route files map to the same URI:~%~{  ~A~%~}"
             (loop for (uri . paths) in (route-conflict-error-conflicts condition)
                   collect (format nil "~S <- ~{~S~^, ~}" uri paths))))))

(defun uri-match-key (uri)
  "Normalise URI so that two URIs matching the same set of incoming paths share
a key. Named parameters collapse to one sentinel, splats to another — so
/users/:id and /users/:name share a key (real conflict), but /docs/:slug and
/docs/* do not (myway dispatches the splat differently)."
  (regex-replace-all ":[^/]+"
                     (regex-replace-all "\\*" uri "<<SPLAT>>")
                     "<<PARAM>>"))

(defun check-route-conflicts (paths)
  "Signal ROUTE-CONFLICT-ERROR when several PATHS resolve to URIs that would
match the same incoming requests (URI-match equivalence, not just string =)."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (path paths)
      (let ((uri (path->uri path)))
        (push (cons path uri) (gethash (uri-match-key uri) groups))))
    (let ((conflicts
            (loop for bucket being the hash-values of groups
                  when (cdr bucket)
                    collect (let ((sorted (sort (copy-list bucket) #'string<
                                                :key #'car)))
                              (cons (cdr (first sorted))
                                    (mapcar #'car sorted))))))
      (when conflicts
        (error 'route-conflict-error
               :conflicts (sort conflicts #'string< :key #'car))))))

(defun install-not-found-handler (app pkg)
  (let ((handler (find-exported-symbol "@NOT-FOUND" pkg)))
    (unless handler
      (error "Package ~A is mapped to /not-found but does not export an ~
              @NOT-FOUND symbol. Did you forget to (:export #:@not-found)?"
             pkg))
    (unless (fboundp handler)
      (error "Package ~A exports @NOT-FOUND but it has no function definition."
             pkg))
    (defmethod ningle:not-found ((current-app (eql app)))
      (declare (ignore current-app))
      (setf (response-status ningle:*response*) 404)
      (funcall handler))))

(defun install-method-handlers (app uri pkg)
  (let ((registered 0))
    (dolist (method *http-request-methods*)
      (let ((handler (find-exported-symbol (concatenate 'string "@" (string method))
                                           pkg)))
        (when handler
          (unless (fboundp handler)
            (error "Package ~A (URI ~A) exports ~A but it has no function ~
                    definition."
                   pkg uri handler))
          (setf (ningle:route app uri :method method) handler)
          (incf registered))))
    (when (zerop registered)
      (error "Route package ~A (URI ~A) exports no handler. ~
              Expected at least one of @GET, @POST, @PUT, @DELETE, …"
             pkg uri))))

(defmethod set-routes ((app ningle:app) &key system dir)
  (let ((paths (detect-paths system dir)))
    (check-route-conflicts paths)
    (loop
      :for path :in paths
      :for uri := (path->uri path)
      :for pkg := (path->package path system dir)
      :do (load-system pkg)
          (if (string= uri "/not-found")
              (install-not-found-handler app pkg)
              (install-method-handlers app uri pkg)))))

(defun list-routes (&key system dir)
  "Return one plist per detected route under SYSTEM and DIR. Keys: :PATH,
:URI, :PACKAGE, :KIND (:STATIC, :DYNAMIC, :CATCH-ALL, or :NOT-FOUND), and
:METHODS (a list of HTTP method keywords, or (:NOT-FOUND) for the special
handler). Each route package is loaded so handlers can be introspected —
same side effect as SET-ROUTES, minus installation into an app."
  (let ((paths (detect-paths system dir)))
    (check-route-conflicts paths)
    (loop for path in paths
          for uri = (path->uri path)
          for pkg = (path->package path system dir)
          do (load-system pkg)
          collect (list :path path
                        :uri uri
                        :package pkg
                        :kind (path-kind path uri)
                        :methods (if (string= uri "/not-found")
                                     (when (find-exported-symbol "@NOT-FOUND" pkg)
                                       '(:not-found))
                                     (exported-methods pkg))))))
