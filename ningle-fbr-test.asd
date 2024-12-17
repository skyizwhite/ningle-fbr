(defsystem "ningle-fbr-test"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "ningle-fbr-test/mapper"
               "ningle-fbr-test/router")
  :perform (test-op (o c) (symbol-call :rove :run c :style :dot)))
