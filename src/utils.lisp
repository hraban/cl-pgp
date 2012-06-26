(in-package :cl-pgp)

(defun surrounded-by-5-dashes-p (str)
  "Evaluates to T if a string is surrounded by exactly five dashes on each side.
  Whitespace is NOT ignored."
  (declare (type string str))
  (let ((len (length str)))
    (and (<= 10 len)
         (string= (subseq str 0 5) "-----")
         (string= (subseq str (- len 5) len) "-----"))))

(5am:test surrounded-by-5-dashes-p
  (5am:is-true (surrounded-by-5-dashes-p "----- foo -----"))
  ;; Whitespace is not ignored
  (5am:is-false (surrounded-by-5-dashes-p " ----- foo ----- "))
  ;; Must be exactly five
  (5am:is-false (surrounded-by-5-dashes-p "-- foo --"))
  ;; Only one true dash character:
  (5am:is-false (surrounded-by-5-dashes-p "_____ foo _____"))
  ;; Empty string is valid (10 dashes)
  (5am:is-true (surrounded-by-5-dashes-p "----------")))

(defun string-trim-whitespace (str)
  (declare (type string str))
  (string-trim '(#\Space #\Tab) str))

(5am:test string-trim-whitespace
  (5am:is (string= "foo" (string-trim-whitespace "  foo  ")))
  (5am:is (string= "" (string-trim-whitespace "")))
  (5am:is (string= "" (string-trim-whitespace "   "))))

(defun split-sequence (raw delimiter &key (count 0) (start 0))
  "Split a sequence by another sequence. Count indicates the maximum number of
  subsequences in the resulting list."
  (if (= count 1)
      (list (subseq raw start))
      (let ((idx (search delimiter raw :start2 start)))
        (if idx
            (cons (subseq raw start idx)
                  (split-sequence raw
                                  delimiter
                                  :count (decf count)
                                  :start (+ idx (length delimiter))))
            (list (subseq raw start))))))

(5am:test split-sequence
  (5am:is (equalp '("ab" "c d" " f " "")
                  (split-sequence "ab--c d-- f --" "--")))
  (5am:is (equalp '((1 2) (3 4 x y 5 x y 6))
                  (split-sequence '(1 2 x y 3 4 x y 5 x y 6)
                                  '(x y)
                                  :count 2))))

(defun assocdr (&rest argv)
  (cdr (apply #'assoc argv)))
