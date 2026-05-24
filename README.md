# ningle-fbr

A file-based router for [ningle](https://github.com/fukamachi/ningle).

Check the [release notes](https://github.com/skyizwhite/ningle-fbr/releases) for the latest updates.

## What is File-Based Routing?

File-based routing automatically creates URL routes based on a project’s file and directory structure. Instead of manually configuring routes in a separate routing file, each file in a designated directory (e.g., `pages` or `routes`) becomes a route. This simplifies development and maintenance since adding, removing, or renaming a route is often just a matter of modifying a file’s name or location.

## Usage

[Practical usage example](https://github.com/skyizwhite/website)

To use ningle-fbr, set up your project using the [package-inferred-system](https://asdf.common-lisp.dev/asdf/The-package_002dinferred_002dsystem-extension.html) style.

**Example directory structure**:
```
example.asd
src/
  app.lisp
  routes/
    index.lisp
    hello.lisp
    users/
      index.lisp
      <id>.lisp
    docs/
      <...>.lisp
    nested/
      page.lisp
```

**`example.asd`**:
```lisp
(defsystem "example"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("example/app"))
```

**`/src/app.lisp`**:
```lisp
(defpackage #:example
  (:nicknames #:example/app)
  (:use #:cl)
  (:import-from #:ningle)
  (:import-from #:ningle-fbr
                #:set-routes))
(in-package #:example/app)

(defparameter *app* (make-instance 'ningle:<app>))

(set-routes *app* :system :example :dir "routes")
```

### Static Routing

Routes are determined by packages located under `:example/routes`. The package’s name corresponds directly to a URL path:

- `:example/routes/index` → `/`
- `:example/routes/hello` → `/hello`
- `:example/routes/users/index` → `/users`
- `:example/routes/nested/page` → `/nested/page`

**`/src/routes/index.lisp`**:
```lisp
(defpackage #:example/routes/index
  (:use #:cl)
  (:export #:@get
           #:@post
           #:@put
           #:@delete))
(in-package #:example/routes/index)

(defun @get (params)
  ;; Implement GET logic here
  )

(defun @post (params)
  ;; Implement POST logic here
  )

(defun @put (params)
  ;; Implement PUT logic here
  )

(defun @delete (params)
  ;; Implement DELETE logic here
  )
```

Handlers are chosen based on the HTTP method. If `@get` is exported, it will be called for `GET` requests on `/`.

### Dynamic Routing

To define dynamic routes, use `<>` in the file name to indicate URL parameters.

For example:
`:example/routes/users/<id>` → `/users/:id`

If a request comes in at `/users/123`, `params` will include `:id "123"`.

### Catch-all (Splat) Routes

Use `<...>` — optionally with a descriptive suffix like `<...slug>` — to match
the remainder of the path, including embedded slashes. The captured value is
delivered under the `:splat` key as a list (one element per `<...>` segment in
the route).

For example:
`:example/routes/docs/<...>` → `/docs/*`

A request to `/docs/intro/getting-started` invokes this route with
`params` containing `(:splat ("intro/getting-started"))`.

```lisp
(defpackage #:example/routes/docs/<...>
  (:use #:cl)
  (:export #:@get))
(in-package #:example/routes/docs/<...>)

(defun @get (params)
  (let ((rest (first (cdr (assoc :splat params)))))
    (format nil "doc: ~A" rest)))
```

### 404 Handling

To handle 404 (Not Found) errors, create a special package named `:example/routes/not-found` and define `@not-found` function:

```lisp
(defpackage #:example/routes/not-found
  (:use #:cl)
  (:export #:@not-found))
(in-package #:example/routes/not-found)

(defun @not-found ()
  ;; Implement custom logic here
  )
```

### Inspecting Routes

`list-routes` returns a structured description of every route file ningle-fbr
detects, without installing anything onto an app. Each entry is a plist with
`:path`, `:uri`, `:package`, `:kind` (`:static`, `:dynamic`, `:catch-all`, or
`:not-found`), and `:methods` (the exported HTTP method handlers).

```lisp
(ningle-fbr:list-routes :system :example :dir "routes")
;; => ((:path "/index"        :uri "/"          :kind :static    :methods (:GET) ...)
;;     (:path "/hello"        :uri "/hello"     :kind :static    :methods (:GET) ...)
;;     (:path "/users/<id>"   :uri "/users/:id" :kind :dynamic   :methods (:GET) ...)
;;     (:path "/docs/<...>"   :uri "/docs/*"    :kind :catch-all :methods (:GET) ...)
;;     (:path "/not-found"    :uri "/not-found" :kind :not-found :methods (:NOT-FOUND) ...))
```

Useful for printing a routes table at boot, generating docs, or sanity-checking
what gets registered.

### Route Conflict Detection

Both `set-routes` and `list-routes` signal `ningle-fbr/router:route-conflict-error`
when two files would match the same set of incoming requests — for example
`users/<id>.lisp` and `users/<name>.lisp`, or `users.lisp` and
`users/index.lisp`. The condition exposes `route-conflict-error-conflicts`, an
alist of `(URI . PATHS)` for programmatic handling.

## License

Licensed under the MIT License.

© 2024 Akira Tempaku