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
  (let* ((reblocks-text-editor/html::*hide-markup-nodes* t)
         (commondoc-markdown/emitter:*emit-section-anchors* nil)
         (commondoc-markdown/emitter:*generate-short-link-references* nil)
         ;; (reblocks-text-editor/html::*render-markup*)
         (result (common-doc.format:emit-to-string (make-instance 'commondoc-markdown:markdown)
                                                   node)))
    (if trim-spaces
        (reblocks-text-editor/utils/text::trim-spaces result)
        result)))


(defun replace-scriba-nodes-with-tmp-placeholders (text)
  (let ((current-id 0)
        (pieces (make-hash-table :test 'equal)))
    (values
     (cl-ppcre:regex-replace-all "@[^ ]+(\\[[^]]*\\])?\\([^)]*\\)" text
                                 (lambda (text start end match-start match-end &rest rest)
                                   (declare (ignore start end rest))
                                   (let* ((matched (subseq text match-start match-end))
                                          (id (incf current-id))
                                          (replacement (format nil "SCRIBA-NODE-~A" id)))
                                     (setf (gethash replacement pieces)
                                           matched)
                                     replacement)))
     pieces)))


(defun replace-placeholders-with-scriba-nodes (text pieces)
  "Makes opposite to REPLACE-SCRIBA-NODES-WITH-TMP-PLACEHOLDERS."
  (cl-ppcre:regex-replace-all "SCRIBA-NODE-[0-9]+" text
                              (lambda (text start end match-start match-end &rest rest)
                                (declare (ignore start end rest))
                                (let* ((matched (subseq text match-start match-end))
                                       (scriba-node (gethash matched pieces)))
                                  (unless scriba-node
                                    (error "Unable to find Scriba node for placeholder ~S." matched))
                                  scriba-node))))


(defun from-markdown (text)
  ;; We need this hack with Scriba nodes replacement because
  ;; otherwise markdown parser will replace nodes like @some[ref=dsd]() with
  ;; a web-link.
  (multiple-value-bind (text-without-scriba-nodes scriba-nodes)
      (replace-scriba-nodes-with-tmp-placeholders text)
    (let ((node (common-doc.format:parse-document (make-instance 'commondoc-markdown:markdown)
                                                  text-without-scriba-nodes)))
      (common-doc.ops:with-document-traversal (node current-node)
        (when (typep current-node 'common-doc:text-node)
          (let ((text (common-doc:text current-node)))
            (setf (common-doc:text current-node)
                  (replace-placeholders-with-scriba-nodes text scriba-nodes)))))
      
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
        (t node)))))
