# ningle-fbr

A file-based router for [ningle](https://github.com/fukamachi/ningle).

## Warning

This software is still in ALPHA quality. The APIs are likely to change.

Please check the [release notes](https://github.com/skyizwhite/ningle-fbr/releases) for updates.

## What is File-Based Routing?

File-based routing is a concept widely used in modern web frameworks like [Next.js](https://nextjs.org/). Instead of explicitly defining routes in code or configuration, routes are automatically generated based on the file and directory structure within a designated folder (often called "pages" or "routes").

## Usage

To use ningle-fbr, you need to set up your project based on the [package-inferred-system](https://asdf.common-lisp.dev/asdf/The-package_002dinferred_002dsystem-extension.html).

`/example.asd`:
```lisp
(defsystem "example"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("example/app"))
```

`/src/app.lisp`:
```lisp
(defpackage #:example
  (:nicknames #:example/app)
  (:use #:cl)
  (:import-from #:ningle)
  (:import-from #:ningle-fbr
                #:set-routes))
(in-package #:example/app)

(defparameter *app* (make-instance 'ningle:<app>))

(set-routes *app* :system :example :target-dir-path "routes")
```

### Static Routing

Routes are generated automatically from packages under `:example/routes`:

- `:example/routes/index` → `/`
- `:example/routes/hello` → `/hello`
- `:example/routes/users/index` → `/users`
- `:example/routes/nested/page` → `/nested/page`

`/src/routes/index.lisp` example:
```lisp
(defpackage #:example/routes/index
  (:use #:cl)
  (:export #:handle-get
           #:handle-post
           #:handle-put
           #:handle-delete))
(in-package #:example/routes/index)

(defun handle-get (params)
  ...)

(defun handle-post (params)
  ...)

(defun handle-put (params)
  ...)

(defun handle-delete (params)
  ...)
```

### Dynamic Routing

Dynamic routes use square brackets to indicate parameters. For example:

`/src/routes/user/[id].lisp` → `/user/[id]`

In the handlers, you can access the value of `id` through the `params` argument.

### Not Found Error

`:example/routes/not-found` is a special package for handling 404 errors. Implement and export `handle-not-found`:

```lisp
(defpackage #:example/routes/not-found
  (:use #:cl)
  (:export #:handle-not-found))
(in-package #:example/routes/not-found)

(defun handle-not-found ()
  ...)
```

## License

Licensed under the MIT License.

© 2024, skyizwhite.