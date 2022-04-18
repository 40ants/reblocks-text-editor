(uiop:define-package #:reblocks-text-editor/typed-pieces/scribdown
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:reblocks-text-editor/typed-pieces/base
                #:typed-piece)
  (:import-from #:reblocks-text-editor/typed-pieces/html
                #:html-piece)
  (:import-from #:reblocks-text-editor/utils/text
                #:remove-html-tags))
(in-package #:reblocks-text-editor/typed-pieces/scribdown)


(defclass scribdown-piece (typed-piece)
  ())


(defmethod reblocks-text-editor/typed-pieces/base::convert ((from html-piece)
                                                            (to (eql :scribdown))
                                                            editable-document
                                                            changed-node)
  (declare (ignore editable-document))
  (let* ((from-text (reblocks-text-editor/typed-pieces/base::document from))
         (to-text (remove-html-tags from-text
                                    :remove-new-lines (not (typep changed-node
                                                                  'common-doc:code-block))))
         (new-caret (reblocks-text-editor/typed-pieces/base::caret from)))
    (cl-ppcre:do-matches (start end "@placeholder.*?\\(\\)"
                          to-text)
      (when (> new-caret start)
        (incf new-caret
              (- end start
                 ;; The virtual length of the replaced noneditable object is 1,
                 ;; that is why we move caret on the length of the placeholder minus 1:
                 1))))
    (make-scribdown-piece to-text new-caret)))


(defun make-scribdown-piece (string caret)
  (check-type string string)
  (check-type caret integer)
  
  (make-instance 'scribdown-piece
                 :document string
                 :caret caret))
