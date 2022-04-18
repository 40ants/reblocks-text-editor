(defsystem "reblocks-text-editor-tests"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("reblocks-text-editor-tests/document/ops"
               "reblocks-text-editor-tests/utils/text"
               "reblocks-text-editor-tests/typed-pieces/scribdown")
  :perform (test-op (o c)
                    (unless (symbol-call :rove '#:run c)
                      (error "Tests failed"))))
