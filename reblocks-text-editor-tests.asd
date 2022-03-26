(defsystem "reblocks-text-editor-tests"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("reblocks-text-editor-tests/document/ops"
               "reblocks-text-editor-tests/utils/text")
  :perform (test-op (o c)
                    (unless (symbol-call :rove '#:run c)
                      (error "Tests failed"))))
