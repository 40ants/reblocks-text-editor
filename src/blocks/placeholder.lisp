(uiop:define-package #:reblocks-text-editor/blocks/placeholder
  (:use #:cl)
  (:import-from #:common-doc
                #:define-node))
(in-package #:reblocks-text-editor/blocks/placeholder)


(define-node placeholder (common-doc:document-node)
             ()
  (:tag-name "placeholder")
  (:documentation "This node will be replaced by a real one with the same reference."))

