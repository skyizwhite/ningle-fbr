(defsystem "ningle-fbr"
  :version "0.1.0"
  :description "File-based router for Ningle"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :author "Akira Tempaku"
  :maintainer "Akira Tempaku <paku@skyizwhite.dev>"
  :license "MIT"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("ningle-fbr/main")
  :in-order-to ((test-op (test-op "ningle-fbr-test"))))
