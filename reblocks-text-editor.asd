(pushnew "~/projects/lisp/common-doc/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/lisp/commondoc-markdown/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/lisp/reblocks/" asdf:*central-registry* :test #'equal)

(defsystem "reblocks-text-editor"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("reblocks-text-editor/editor")
  :in-order-to ((test-op (test-op "reblocks-text-editor-tests"))))


(register-system-packages "common-doc" '("COMMON-DOC.FORMAT"))
(register-system-packages "scriba" '("SCRIBA.EMITTER"))
