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
  "Translate an absolute file PATHNAME located under DIR-PATHNAME into a
leading-slash URL-like path. e.g. /…/routes/users/<id>.lisp under /…/routes/
becomes \"/users/<id>\"."
  (let* ((file-directory (rest (pathname-directory pathname)))
         (base-directory (rest (pathname-directory dir-pathname)))
         (relative-directory (subseq file-directory (length base-directory))))
    (format nil "/~{~A/~}~A" relative-directory (pathname-name pathname))))

(defun catch-all-path-p (path)
  "True when PATH contains a catch-all segment (\"<...>\" or \"<...name>\")."
  (and (search "<..." path) t))

(defun dynamic-path-p (path)
  "True when PATH contains any bracketed segment — covers both named
parameters (\"<id>\") and catch-alls (\"<...>\")."
  (and (search "<" path) t))

(defun path-precedence-key (path)
  "Numeric sort key encoding route specificity: static (0) < dynamic (1) <
catch-all (2). myway dispatches in registration order, so more-specific
routes must be installed first."
  (cond ((catch-all-path-p path) 2)
        ((dynamic-path-p path) 1)
        (t 0)))

(defun path-precedence< (a b)
  "Strict ordering on paths used as the sort predicate during route discovery:
specificity first via PATH-PRECEDENCE-KEY, with ties broken lexicographically
so the ordering is total and stable across runs."
  (let ((key-a (path-precedence-key a))
        (key-b (path-precedence-key b)))
    (cond ((< key-a key-b) t)
          ((> key-a key-b) nil)
          (t (string< a b)))))

(defun discover-route-paths (system dir)
  "Walk DIR (relative to the source root of ASDF SYSTEM) and return every
*.lisp file in it as a route path, sorted by PATH-PRECEDENCE< so that
more-specific routes come first."
  (let* ((dir-pathname
           (merge-pathnames (concatenate 'string dir "/")
                            (asdf:component-pathname (asdf:find-system system))))
         (paths (mapcar (lambda (pathname)
                          (pathname->path pathname dir-pathname))
                        (directory (merge-pathnames "**/*.lisp" dir-pathname)))))
    (sort paths #'path-precedence<)))

(defun strip-index-segment (path)
  "Collapse a trailing \"/index\" segment so directories addressed by their
index file expose the bare directory URI. \"/index\" → \"/\" and
\"/users/index\" → \"/users\"; all other paths pass through unchanged."
  (if (string= path "/index")
      "/"
      (regex-replace "/index$" path "")))

(defun bracket->myway (path)
  "Translate file-based parameter syntax into myway's URL grammar:
  <...> or <...name> → *      (catch-all, surfaced as :SPLAT in params)
  <name>             → :name  (named parameter)
The catch-all transformation runs first so that <...rest> is not mis-parsed
as the named parameter :...rest."
  (let ((after-catch-all (regex-replace-all "<\\.\\.\\.[^>]*>" path "*")))
    (regex-replace-all "<([^>]+)>" after-catch-all ":\\1")))

(defun path->uri (path)
  "Convert a route PATH (as produced by PATHNAME->PATH) into the URI string
that ningle/myway will match against incoming requests: collapses /index
segments and rewrites bracketed parameters to myway's :name/* syntax."
  (bracket->myway (strip-index-segment path)))

(defun path-kind (path uri)
  "Classify a detected route file as :STATIC, :DYNAMIC, :CATCH-ALL, or
:NOT-FOUND. URI is needed to recognise the special /not-found handler;
specificity (catch-all before dynamic before static) is derived from PATH."
  (cond ((string= uri "/not-found") :not-found)
        ((catch-all-path-p path) :catch-all)
        ((dynamic-path-p path) :dynamic)
        (t :static)))

(defun path->package (path system dir)
  "Derive the keyword naming the package that ningle-fbr expects to find a
route's handlers in: \"SYSTEM/DIR/PATH\" upcased into a keyword. e.g. path
\"/users/<id>\" under system :APP, dir \"routes\" → :APP/ROUTES/USERS/<ID>."
  (make-keyword (string-upcase (concatenate 'string
                                            (string system)
                                            "/"
                                            dir
                                            path))))

(defparameter *http-request-methods*
  '(:GET :POST :PUT :DELETE :HEAD :CONNECT :OPTIONS :PATCH :TRACE)
  "HTTP request methods scanned for on each route package. For every method M
in this list, a handler is looked up as the external symbol named \"@M\"
(e.g. @GET, @POST). New methods can be added here without touching the
installation logic.")

(defun find-exported-symbol (name pkg)
  "Return the symbol named NAME in PKG only if it is exported, otherwise NIL.
Used so we never install handlers that aren't part of a route package's
public interface — interned-but-not-exported symbols are treated as absent."
  (multiple-value-bind (symbol status) (find-symbol name pkg)
    (and (eq status :external) symbol)))

(defun exported-http-methods (pkg)
  "List the HTTP method keywords for which PKG exports a handler symbol
(\"@GET\", \"@POST\", …). The order of the returned list mirrors
*HTTP-REQUEST-METHODS*."
  (loop for method in *http-request-methods*
        when (find-exported-symbol (concatenate 'string "@" (string method)) pkg)
          collect method))

(define-condition route-definition-error (error) ()
  (:documentation "Base condition for problems detected while loading or
installing routes — conflicts, missing handlers, unbound symbols, etc."))

(define-condition route-conflict-error (route-definition-error)
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

(define-condition missing-not-found-handler (route-definition-error)
  ((package :initarg :package
            :reader missing-not-found-handler-package
            :documentation "The route package mapped to /not-found that fails
to export an @NOT-FOUND symbol."))
  (:report
   (lambda (condition stream)
     (format stream
             "Route package ~A is mapped to /not-found but does not export an ~
              @NOT-FOUND symbol. Did you forget to (:export #:@not-found)?"
             (missing-not-found-handler-package condition))))
  (:documentation "Signalled when a /not-found route package is detected but
no @NOT-FOUND symbol is exported from it."))

(define-condition missing-method-handlers (route-definition-error)
  ((package :initarg :package
            :reader missing-method-handlers-package
            :documentation "The route package that exports no HTTP method
handlers.")
   (uri :initarg :uri
        :reader missing-method-handlers-uri
        :documentation "The URI this package was mapped to."))
  (:report
   (lambda (condition stream)
     (format stream
             "Route package ~A (URI ~A) exports no handler. ~
              Expected at least one of @GET, @POST, @PUT, @DELETE, …"
             (missing-method-handlers-package condition)
             (missing-method-handlers-uri condition))))
  (:documentation "Signalled when a non-/not-found route package is detected
but exports none of the recognised @METHOD handler symbols."))

(define-condition unbound-route-handler (route-definition-error)
  ((package :initarg :package
            :reader unbound-route-handler-package
            :documentation "The route package owning the unbound handler
symbol.")
   (uri :initarg :uri
        :initform nil
        :reader unbound-route-handler-uri
        :documentation "URI the handler was being installed for, or NIL for
the /not-found handler.")
   (handler :initarg :handler
            :reader unbound-route-handler-handler
            :documentation "The exported but unbound handler symbol."))
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
  (:documentation "Signalled when a route package exports an @METHOD or
@NOT-FOUND symbol that is not FBOUNDP — usually because the handler
function was never DEFUN'd or the file failed to load cleanly."))

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
match the same incoming requests (URI-match equivalence per URI-MATCH-KEY,
not just string =). The resulting condition lists every colliding group
with its representative URI and the paths in it."
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
  "Wire PKG's exported @NOT-FOUND function into APP as the NINGLE:NOT-FOUND
method, also forcing the response status to 404. Signals
MISSING-NOT-FOUND-HANDLER if the symbol is not exported and
UNBOUND-ROUTE-HANDLER if it is exported but has no function binding."
  (let ((handler (find-exported-symbol "@NOT-FOUND" pkg)))
    (unless handler
      (error 'missing-not-found-handler :package pkg))
    (unless (fboundp handler)
      (error 'unbound-route-handler :package pkg :handler handler))
    (defmethod ningle:not-found ((current-app (eql app)))
      (declare (ignore current-app))
      (setf (response-status ningle:*response*) 404)
      (funcall handler))))

(defun install-method-handlers (app uri pkg)
  "Register every @METHOD handler exported by PKG as a route on APP at URI.
Signals UNBOUND-ROUTE-HANDLER if an exported handler symbol has no function
binding, and MISSING-METHOD-HANDLERS if PKG exports no recognised handler
at all."
  (let ((registered-count 0))
    (dolist (method *http-request-methods*)
      (let ((handler (find-exported-symbol (concatenate 'string "@" (string method))
                                           pkg)))
        (when handler
          (unless (fboundp handler)
            (error 'unbound-route-handler
                   :package pkg :uri uri :handler handler))
          (setf (ningle:route app uri :method method) handler)
          (incf registered-count))))
    (when (zerop registered-count)
      (error 'missing-method-handlers :package pkg :uri uri))))

(defmethod set-routes ((app ningle:app) &key system dir)
  "Discover every route file under DIR (relative to ASDF SYSTEM's source
root), load its package, and install its handlers onto APP. Routes are
registered in specificity order — static before dynamic before catch-all —
so myway's first-match dispatch favours the more specific route. The
package named SYSTEM/DIR/not-found, if present, is installed as the custom
404 handler instead. Signals a ROUTE-DEFINITION-ERROR subclass on
conflicts, missing handlers, or unbound handler symbols."
  (let ((paths (discover-route-paths system dir)))
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
  (let ((paths (discover-route-paths system dir)))
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
                                     (exported-http-methods pkg))))))
