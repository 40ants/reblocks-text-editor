(defsystem "reblocks-text-editor-tests"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("reblocks-text-editor-tests/document/ops")
  :perform (test-op (o c)
                    (unless (symbol-call :rove '#:run c)
                      (error "Tests failed"))))
