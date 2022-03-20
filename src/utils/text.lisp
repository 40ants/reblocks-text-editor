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


(defun remove-html-tags (html-string &key (remove-new-lines t))
  (let* ((result (cl-ppcre:regex-replace-all "<[^>]+>" html-string
                                             ""))
         (result (if (string= result +zero-width-space+)
                     result
                     (cl-ppcre:regex-replace-all +zero-width-space+
                                                 result
                                                 "")))
         (result (plump:decode-entities result))
         (result (if remove-new-lines
                     ;; For some strange reason sometimes browser starts
                     ;; passing newlines even inside span elements. Why? Don't know.
                     ;; Thus by default we are removing new lines.
                     ;; However, when processing content of code blocks
                     ;; it is useful to keep newlines.
                     (str:replace-all '(#\Newline) "" result)
                     result)))
    result))

