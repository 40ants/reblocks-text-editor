(uiop:define-package #:reblocks-text-editor-tests/utils/text
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:deftest)
  (:import-from #:reblocks-text-editor/utils/text
                #:remove-html-tags
                #:caret-position-from-beginning-of-the-line
                #:move-caret-on-the-next-line))
(in-package #:reblocks-text-editor-tests/utils/text)


(deftest test-move-caret-on-the-next-line
  (let ((pos (move-caret-on-the-next-line
              "Block of code
second line
middle line
last line" 3)))
    (ok (= pos 17)))
  
  (let ((pos (move-caret-on-the-next-line
              "Block of code
second line
middle line
last line" 17)))
    (ok (= pos 29))))


(deftest test-caret-position-from-beginning-of-the-line
    (let ((pos (caret-position-from-beginning-of-the-line
                "Block of code
  second line
  middle line
  last line" 3)))
      (ok (= pos 3)))
  
  (let ((pos (caret-position-from-beginning-of-the-line
              "Block of code
second line
middle line
last line" 17)))
    (ok (= pos 3))))



(deftest test-remove-html-tags-1
  (let ((result (remove-html-tags "<i><strong>Text</strong> other</i>"))
        (expected "Text other"))
    (ok (equal result expected))))


(deftest test-remove-html-tags-2
  "It should replace noneditable nodes with a Scriba placeholder."
  (let ((result (remove-html-tags "<i><img src=\"foo\" class=\"noneditable\" id=\"some-ref\"/> other</i>"))
        (expected "@placeholder[ref=some-ref]() other"))
    (ok (equal result expected))))
