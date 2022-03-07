(uiop:define-package #:reblocks-text-editor/utils/markdown
  (:use #:cl)
  (:import-from #:3bmd)
  (:import-from #:commondoc-markdown/emitter)
  (:import-from #:common-doc.format)
  (:import-from #:common-doc)
  (:import-from #:reblocks-text-editor/utils/text)
  (:import-from #:reblocks-text-editor/html))
(in-package #:reblocks-text-editor/utils/markdown)


;; Here we need to redefine this rule, to prevent
;; 3bmd to parse [such] contructs as a link :(
(esrap:defrule 3bmd-grammar::reference-link-single "PleAsE DoNT"
  (:constant :ignore))


(defun to-markdown (node &key (trim-spaces t))
  "Probably you want to set TRIM-SPACES to NIL when you are
   rendering to markdown a node which already existed in the
   document, like a node before the a text to be pasted.

   If you don't do this, then a space before the cursor might be lost."
  (let* ((commondoc-markdown/emitter:*emit-section-anchors* nil)
         (commondoc-markdown/emitter:*generate-short-link-references* nil)
         (reblocks-text-editor/html::*render-markup*)
         (result (common-doc.format:emit-to-string (make-instance 'commondoc-markdown:markdown)
                                                   node)))
    (if trim-spaces
        (reblocks-text-editor/utils/text::trim-spaces result)
        result)))


(defun from-markdown (text)
  (let ((node (common-doc.format:parse-document (make-instance 'commondoc-markdown:markdown)
                                                text)))
    ;; This is a workaround for a bug inside commondoc-markdown or 3bmd
    ;; When it parses an empty text, it returns an empty CONTENT-NODE,
    ;; But we need a PARAGRAPH.
    (cond
      ((and (typep node 'common-doc:content-node)
            (null (common-doc:children node)))
       (common-doc:make-paragraph
        (list
         ;; Without this hack with &ZeroWidthSpace;
         ;; we'll be unable to put cursor into a new paragraph :(
         (common-doc:make-text reblocks-text-editor/utils/text::+zero-width-space+))))
      (t node))))
