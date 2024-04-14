# ningle-fbr (WIP)

An utility for [ningle](https://github.com/fukamachi/ningle) and [jingle](https://github.com/dnaeon/cl-jingle) to enable file-based routing

# What is file-based routing?

File-based routing is a concept commonly used in modern web frameworks such as [Next.js](https://nextjs.org/). Instead of explicitly defining routes through configuration or code, the framework automatically sets up routes based on the file hierarchy of a particular directory (usually the "pages" or "routes" directory).

# Usage

To use ningle-fbr, you must use [package-inferred-system](https://asdf.common-lisp.dev/asdf/The-package_002dinferred_002dsystem-extension.html).

`/example.asd`
```lisp
(defsystem "example"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("example/app")
```

`/src/app.lisp`
```lisp
(uiop:define-package #:example
  (:nicknames #:example/app)
  (:use #:cl)
  (:import-from #:ningle)
  (:import-from #:ningle-fbr
　　　　　　　　　 #:assign-routes))
(in-package #:example/app)

(defparameter *app* (make-instance 'ningle:<app>))

(assign-routes *app*
               :directory "src/routes"
               :system "example")
```

## Static routing

`/src/routes/index.lisp` → `/`

`/src/routes/hello.lisp` → `/hello`

`/src/routes/users/index.lisp` → `/users`

`/src/routes/nested/page.lisp` → `/nested/page`

```lisp
(uiop:define-package #:example/routes/index
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

## Dynamic routing

A file or directory name prefixed with '=' indicates a dynamic path. 

In the example below, the parameter `id` can be obtained from handler's params.

`/src/routes/user/=id.lisp` → `/user/:id`

# License

Licensed under MIT License.　

Copyright (c) 2024, skyizwhite.
