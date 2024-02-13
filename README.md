# ningle-fbr (WIP)
Plugin for [ningle](https://github.com/fukamachi/ningle) and [jingle](https://github.com/dnaeon/cl-jingle) to enable file-based routing

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
  (:import-from #:ningle-fbr))
(in-package #:example/app)

(defparameter *app* (make-instance 'ningle:<app>))

(defun update-routes ()
  (ningle-fbr:enable-file-based-routing
    *app*
    :dir "src/routes"
    :system "example"
    :system-pathname "src"))

(update-routes)
```

## Static routing

`/src/routes/index.lisp` → `/`
```lisp
(uiop:define-package #:example/routes/index
  (:use #:cl)
  (:export #:on-get
           #:on-post
           #:on-put
           #:on-delete))
(in-package #:example/routes/index)

(defun on-get (params)
  ...)

(defun on-post (params)
  ...)

(defun on-put (params)
  ...)

(defun on-delete (params)
  ...)
```

## Dynamic routing

A file or directory name prefixed with '=' indicates a dynamic path. 

In the example below, the name parameter can be obtained from handler's params.

`/src/routes/user/=name.lisp` → `/user/:name`

# License

Licensed under MIT License.　

Copyright (c) 2024, skyizwhite.
