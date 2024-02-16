(defsystem "ningle-fbr-tests"
  :author "skyizwhite <paku@skyizwhite.dev>"
  :license "MIT"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "ningle-fbr-tests/main")
  :perform (test-op (o c)
                    (symbol-call :rove '#:run c)))
