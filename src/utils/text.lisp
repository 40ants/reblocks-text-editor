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

(defun remove-zero-spaces-unless-string-is-empty (string)
  (cond
    ((string= string "")
     +zero-width-space+)
    ((string= string +zero-width-space+)
     string)
    (t
     (cl-ppcre:regex-replace-all +zero-width-space+
                                 string
                                 ""))))


(defun remove-html-tags (html-string &key (remove-new-lines t))
  (let* ((result (cl-ppcre:regex-replace-all "<[^>]+>" html-string
                                             ""))
         (result (remove-zero-spaces-unless-string-is-empty result))
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


(defun ensure-two-newlines-at-the-end (string)
  (if (and (>= (length string)
               2)
           (eql (elt string (1- (length string)))
                #\Newline)
           (not (eql (elt string (1- (1- (length string))))
                     #\Newline)))
      (with-output-to-string (s)
        (write-string string s)
        (write-char #\Newline s))
      string)
  
  ;; (let ((num-newlines-to-add
  ;;         (+ (if (and (>= (length string)
  ;;                         1)
  ;;                     (not (eql (elt string (1- (length string)))
  ;;                               #\Newline)))
  ;;                1
  ;;                0)
              
  ;;             (if (and (>= (length string)
  ;;                          2)
  ;;                      (not (eql (elt string (1- (1- (length string))))
  ;;                                #\Newline))
  ;;                      (not (eql (elt string (1- (1- (length string))))
  ;;                                #\Newline)))))
  ;;         )) (if 
  ;;             (with-output-to-string (s)
  ;;               (write-string string s)
  ;;               (write-char #\Newline s)
  ;;               (write-char #\Newline s))
  ;;             string))
  )
