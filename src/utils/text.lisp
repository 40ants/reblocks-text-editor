(uiop:define-package #:reblocks-text-editor/utils/text
  (:use #:cl)
  (:import-from #:cl-ppcre))
(in-package #:reblocks-text-editor/utils/text)


(defparameter +zero-width-space+
  (coerce '(#\Zero_Width_Space) 'string)
  "This string contains a character which corresponds to a &ZeroWidthSpace; HTML entity.")


(defun trim-spaces (string)
  (string-trim '(#\Newline #\Space #\Tab #\Zero_Width_Space)
               string))


(defun remove-html-tags (html-string)
  (let ((result (cl-ppcre:regex-replace-all "<[^>]+>" html-string
                                            "")))
    (plump:decode-entities
     (if (string= result +zero-width-space+)
         result
         (cl-ppcre:regex-replace-all +zero-width-space+
                                     result
                                     "")))))

