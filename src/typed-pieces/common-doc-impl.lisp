(uiop:define-package #:reblocks-text-editor/typed-pieces/common-doc-impl
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:reblocks-text-editor/typed-pieces/base
                #:typed-piece
                #:caret
                #:document)
  (:import-from #:reblocks-text-editor/typed-pieces/html
                #:html-piece)
  (:import-from #:reblocks-text-editor/utils/text
                #:+zero-width-space+
                #:remove-html-tags)
  (:import-from #:cl-ppcre)
  (:import-from #:reblocks-text-editor/document/ops)
  (:import-from #:reblocks-text-editor/typed-pieces/scribdown
                #:scribdown-piece)
  (:import-from #:reblocks-text-editor/typed-pieces/common-doc
                #:common-doc-piece
                #:make-common-doc-piece))
(in-package #:reblocks-text-editor/typed-pieces/common-doc-impl)


(defun decrement-of-placeholders-before-caret (content caret-position)
  "Counts characters taken by text like \"Image попробуем в середине строки: @placeholder[ref=some]()\"
   inside the CONTENT.
   Returns sum of each placeholder minus number of placeholders, because
   each noneditable object \"has\" length of 1 character."
  (check-type content string)
  (check-type caret-position integer)
  (loop with matches = (cl-ppcre:all-matches "@placeholder\\[[^]]*\\]\\([^)]*\\)" content
                                             :end (min caret-position
                                                       (length content)))
        for (left right) on matches by #'cddr
        summing (- right left 1)))


(defun guess-caret-position-decrement (content)
  "Returns a number of symbols, which will disappear if some content
   in the CONTENT string will be replaced by a noneditable block
   like an image."
  (check-type content scribdown-piece)
  
  (let* ((text (document content))
         (caret-position (caret content))
         (placeholders-decrement
           (decrement-of-placeholders-before-caret text
                                                   caret-position)))
    (destructuring-bind (&optional left right)
        (cl-ppcre:all-matches "!\\[[^]]*\\]\\([^)]*\\)$" text
                              :end (min caret-position
                                        (length text)))
      (when (and left right)
        (return-from guess-caret-position-decrement
          ;; Image was inserted and image node has length of 1
          ;; that is why we are making 1- here
          (+ (- right left 1)
             placeholders-decrement))))

    ;; If nothing matched:
    placeholders-decrement))


(defun markup-caret-position-decrement (root-node caret)
  "Calculates how many markup characters were before caret
   in the scribdown text."
  (check-type root-node common-doc:document-node)
  (check-type caret integer)
  
  (let (;; (reblocks-text-editor/html::*render-markup* t)
        (current-pos caret)
        (decrement 0))
    (flet ((walk (node depth)
             (declare (ignore depth))
             ;; (format t "TRACE: ~A: pos: ~A, dec: ~A (~A)~%"
             ;;         node
             ;;         current-pos
             ;;         decrement
             ;;         reblocks-text-editor/html::*render-markup*)
             (typecase node
               (reblocks-text-editor/html/markup::markup
                (let ((len (length (common-doc:text node))))
                  (cond
                    ((< current-pos len)
                     (incf decrement current-pos)
                     ;; (format t "TRACE: Exit from markup: pos: ~A, dec: ~A~%"
                     ;;         current-pos
                     ;;         decrement)
                     (return-from markup-caret-position-decrement
                       decrement))
                    (t
                     (incf decrement len)
                     (decf current-pos len)
                     ;; (format t "TRACE: changed to: pos: ~A, dec: ~A~%"
                     ;;         current-pos
                     ;;         decrement)
                     ))))
               (common-doc:text-node
                (let ((len (length (common-doc:text node))))
                  (cond
                    ((< current-pos len)
                     ;; (format t "TRACE: Exit from text: pos: ~A, dec: ~A~%"
                     ;;         current-pos
                     ;;         decrement)
                     (return-from markup-caret-position-decrement
                       decrement))
                    (t
                     (decf current-pos len)
                     ;; (format t "TRACE: changed to: pos: ~A~%"
                     ;;         current-pos)
                     )))))
             node))
      (reblocks-text-editor/document/ops::map-document root-node #'walk 0 nil
                                                       ;; including-markup
                                                       t)
      ;; (format t "TRACE: Normal exit: pos: ~A, dec: ~A~%"
      ;;         current-pos
      ;;         decrement)
      (values decrement))))


(defun calculate-caret-position-for-html (piece)
  "Does opposite to MARKUP-CARET-POSITION-DECREMENT and calculates
   caret position for representation which will be shown to a user.

   Here we need to increment caret position on number of
   markup characters before the caret."
  (check-type piece common-doc-piece)

  (let (;; (reblocks-text-editor/html::*render-markup* t)
        (current-pos (caret piece))
        (chars-left (caret piece)))
    (flet ((walk (node depth)
             (declare (ignore depth))
             ;; (format t "TRACE: ~A: pos: ~A~%"
             ;;         node
             ;;         current-pos)
             (typecase node
               (reblocks-text-editor/html/markup::markup
                (let ((len (length (common-doc:text node))))
                  (cond
                    ((<= current-pos len)
                     ;; Jump to position after the markup
                     (incf current-pos len)
                     ;; (format t "TRACE: changed to: pos: ~A~%"
                     ;;         current-pos)
                     )
                    (t
                     (incf current-pos len)
                     ;; (format t "TRACE: changed to: pos: ~A~%"
                     ;;         current-pos)
                     ))))
               ;; TODO: process image here as well
               ;; other noneditable blocks
               (common-doc:text-node
                (let ((len (length (common-doc:text node))))
                  (cond
                    ((<= chars-left len)
                     ;; (format t "TRACE: Exit from text: pos: ~A~%"
                     ;;         current-pos)
                     (return-from calculate-caret-position-for-html
                       current-pos))
                    (t
                     (decf chars-left len)
                     ;; (format t "TRACE: ignoring node and staying at: pos: ~A~%"
                     ;;         current-pos)
                     )))))
             node))
      (reblocks-text-editor/document/ops::map-document (document piece) #'walk 0 nil
                                                       ;; including-markup
                                                       t)
      ;; (format t "TRACE: Normal exit: pos: ~A~%"
      ;;         current-pos)
      (values current-pos))))


(defmethod reblocks-text-editor/typed-pieces/base::convert ((from scribdown-piece)
                                                            (to (eql :common-doc))
                                                            editable-document
                                                            changed-node)
  (let* ((cursor-position-decrement ;; 0
           (guess-caret-position-decrement from))
         (processed-content
           (reblocks-text-editor/document/ops::prepare-new-content editable-document
                                                                   (document from)))
         (children (common-doc::children processed-content))
         (last-child (car (last children))))
         
    (when (typep last-child 'common-doc:image)
      (let ((empty-node (common-doc:make-text +zero-width-space+)))
        (reblocks-text-editor/document/ops::add-reference-ids editable-document
                                                              :to-node empty-node)
        (setf (common-doc::children processed-content)
              (append (common-doc:children processed-content)
                      (list empty-node))))
      (decf cursor-position-decrement))

    ;; TODO: with real markup nodes we don't need this:
    ;; (incf cursor-position-decrement
    ;;       (markup-caret-position-decrement processed-content
    ;;                                        (caret from)))
    
    (make-common-doc-piece processed-content
                           ;; When are replacing text entered by a user
                           ;; with a noneditable node like image,
                           ;; caret position should be decreased to the
                           ;; length of this entered text, because now
                           ;; we are removing it from the editable area.
                           ;; 
                           ;; Here we are using a hack to guess how many
                           ;; characters was removed by PREPARE-NEW-CONTENT:
                           (- (caret from)
                              cursor-position-decrement))))
