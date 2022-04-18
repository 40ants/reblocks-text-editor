(uiop:define-package #:reblocks-text-editor/typed-pieces/html
  (:use #:cl)
  (:import-from #:reblocks-text-editor/typed-pieces/base
                #:typed-piece))
(in-package #:reblocks-text-editor/typed-pieces/html)


(defclass html-piece (typed-piece)
  ())


(defun make-html-piece (string caret)
  (check-type string string)
  (check-type caret integer)
  
  (make-instance 'html-piece
                 :document string
                 :caret caret))
