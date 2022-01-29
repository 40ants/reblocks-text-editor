(pushnew "~/projects/lisp/common-doc/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/lisp/commondoc-markdown/" asdf:*central-registry* :test #'equal)
(pushnew "~/projects/lisp/reblocks/" asdf:*central-registry* :test #'equal)

(defsystem "zibaldone"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("zibaldone/widgets/editor"))
