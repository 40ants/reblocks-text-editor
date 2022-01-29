(pushnew "~/projects/lisp/common-doc/" asdf:*central-registry* :test #'string=)
(pushnew "~/projects/lisp/commondoc-markdown/" asdf:*central-registry* :test #'string=)

(defsystem "zibaldone"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("zibaldone/widgets/editor"))
