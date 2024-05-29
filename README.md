# ningle-fbr (WIP)

A utility for [ningle](https://github.com/fukamachi/ningle) and [jingle](https://github.com/dnaeon/cl-jingle) to enable file-based routing.

## What is File-Based Routing?

File-based routing is a concept commonly used in modern web frameworks such as [Next.js](https://nextjs.org/). Instead of explicitly defining routes through configuration or code, the framework automatically sets up routes based on the file hierarchy of a specific directory (usually the "pages" or "routes" directory).

## Usage

To use ningle-fbr, you need to use [package-inferred-system](https://asdf.common-lisp.dev/asdf/The-package_002dinferred_002dsystem-extension.html).

`/example.asd`
```lisp
(defsystem "example"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("example/app"))
```

`/src/app.lisp`
```lisp
(defpackage #:example
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

### Static Routing

`/src/routes/index.lisp` → `/`

`/src/routes/hello.lisp` → `/hello`

`/src/routes/users/index.lisp` → `/users`

`/src/routes/nested/page.lisp` → `/nested/page`

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

A file or directory name prefixed with '=' indicates a dynamic path.

In the example below, the parameter `id` can be obtained from the handler's params.

`/src/routes/user/=id.lisp` → `/user/:id`

### Not Found Error

`not-found.lisp` is a special file to handle 404 errors. Implement the `handle-not-found` function and export it.

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
