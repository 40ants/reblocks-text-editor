(uiop:define-package #:reblocks-text-editor/typed-pieces/common-doc
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:reblocks-text-editor/typed-pieces/base
                #:typed-piece
                #:caret
                #:document))
(in-package #:reblocks-text-editor/typed-pieces/common-doc)


(defclass common-doc-piece (typed-piece)
  ())


(defun make-common-doc-piece (root-node caret)
  (check-type root-node common-doc:document-node)
  (check-type caret integer)
  
  (make-instance 'common-doc-piece
                 :document root-node
                 :caret caret))
