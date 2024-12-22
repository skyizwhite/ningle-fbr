# ningle-fbr

A file-based router for [ningle](https://github.com/fukamachi/ningle).

## Warning

This software is still in ALPHA quality. The APIs are likely to change.

Please check the [release notes](https://github.com/skyizwhite/ningle-fbr/releases) for updates.

## What is File-Based Routing?

File-based routing automatically generates URL routes based on a project’s file and directory structure. Instead of manually configuring routes in a separate routing file, each file in a designated directory (e.g., `pages` or `routes`) becomes a route. This simplifies development and maintenance since adding, removing, or renaming a route is often just a matter of modifying a file’s name or location.

## Usage

To use ningle-fbr, set up your project in accordance with the [package-inferred-system](https://asdf.common-lisp.dev/asdf/The-package_002dinferred_002dsystem-extension.html) conventions.

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
  (:import-from #:ningle-fbr #:set-routes))
(in-package #:example/app)

(defparameter *app* (make-instance 'ningle:<app>))

(set-routes *app* :system :example :target-dir-path "routes")
```

### Static Routing

Routes are derived from packages under `:example/routes`. The package’s name corresponds directly to a URL path:

- `:example/routes/index` → `/`
- `:example/routes/hello` → `/hello`
- `:example/routes/users/index` → `/users`
- `:example/routes/nested/page` → `/nested/page`

**`/src/routes/index.lisp`**:
```lisp
(defpackage #:example/routes/index
  (:use #:cl)
  (:export #:handle-get
           #:handle-post
           #:handle-put
           #:handle-delete))
(in-package #:example/routes/index)

(defun handle-get (params)
  ;; implement GET logic here
  )

(defun handle-post (params)
  ;; implement POST logic here
  )

(defun handle-put (params)
  ;; implement PUT logic here
  )

(defun handle-delete (params)
  ;; implement DELETE logic here
  )
```

Handlers are chosen based on the HTTP method. If `handle-get` is exported, it will be called for `GET` requests on `/`.

### Dynamic Routing

To define dynamic routes, use `<>` in the file name to indicate URL parameters.

For example:
`:example/routes/user/<id>` → `/user/:id`

If a request comes in at `/user/123`, `params` will include `:id "123"`.

### 404 Handling

To handle 404 (Not Found) error, create a special package named `:example/routes/not-found` and define `handle-not-found`:

```lisp
(defpackage #:example/routes/not-found
  (:use #:cl)
  (:export #:handle-not-found))
(in-package #:example/routes/not-found)

(defun handle-not-found ()
  ;; Implement custom 404 logic here
  )
```

## License

Licensed under the MIT License.

© 2024, skyizwhite.