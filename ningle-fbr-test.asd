(defsystem "ningle-fbr-test"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "ningle-fbr-test/uri-mapper"
               "ningle-fbr-test/package-mapper"
               "ningle-fbr-test/router")
  :perform (test-op (o c) (symbol-call :rove :run c :style :dot)))
