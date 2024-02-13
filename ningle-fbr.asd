(defsystem "ningle-fbr"
  :version "0.1.0"
  :description "Plugin for ningle to enable file-based routing"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :author "skyizwhite <paku@skyizwhite.dev>"
  :license "MIT"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("ningle-fbr/main"))
