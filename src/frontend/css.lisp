(uiop:define-package #:reblocks-text-editor/frontend/css
  (:use #:cl)
  (:import-from #:reblocks-lass))
(in-package #:reblocks-text-editor/frontend/css)


(defun make-css-code ()
  (reblocks-lass:make-dependency
    '(body
      (.editor
       (.content :outline none)
       (.content
        (p
         :white-space pre-wrap)
        (ul
         :padding-left 1.2rem))
       (.bold :font-weight bold)
       (.markup :display none)
       (code
        :border inherit
        :background-color inherit
        :padding 0)
       (.code-block
        :background-color "#EEE"
        :margin-left -0.5rem
        :padding-left 0.5rem
        :margin-right -0.5rem
        :padding-right 0.5rem)
       ((:and p .active)
        (.markup :display inline-block))))))
