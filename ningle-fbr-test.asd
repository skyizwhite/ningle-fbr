(defsystem "ningle-fbr-test"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "ningle-fbr-test/main")
  :perform (test-op (o c) (symbol-call :rove :run c :style :dot)))
