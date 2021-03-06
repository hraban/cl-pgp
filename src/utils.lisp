(in-package :cl-pgp)

(defmacro force-string (x)
  `(coerce ,x 'string))

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
  (string-trim '(#\Space #\Tab #\Linefeed #\Return) str))

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

(defun newlinep (c)
  (declare (type character c))
  (or (char= c #\Linefeed) (char= c #\Return)))

(defun string-empty-p (s)
  (declare (type string s))
  (string= s ""))

(defun string-starts-with-p (whole start)
  (declare (type string whole start))
  (let ((init-len (length start)))
    (when (>= (length whole) init-len)
      (string= (subseq whole 0 init-len) start))))

(5am:test string-starts-with-p
  (5am:is-true (string-starts-with-p "foo bar" "foo"))
  (5am:is-true (string-starts-with-p "foo" "foo"))
  (5am:is-true (string-starts-with-p "foo" ""))
  (5am:is-false (string-starts-with-p "foo" "foo bar")))

(defparameter *crlf*
  (the string (coerce #(#\Return #\Newline) 'string)))

(defun string-join (strings &optional (delim ""))
  "Join a sequence of strings into one string"
  (declare (type list strings))
  (let ((fmt (format NIL "~~{~~a~~^~a~~}" delim)))
    (format NIL fmt strings)))

(5am:test string-join
  (5am:is (string= "a b c" (string-join '("a" "b" "c") " ")))
  (5am:is (string= "foo" (string-join '("f" "o" "o"))))
  (5am:is (string= "" (string-join '()))))

(defun unix2dos (s)
  "Replace all occurences of #\Linefeed by #\Return #\Linefeed"
  (declare (type string s))
  (string-join (split-sequence:split-sequence #\Linefeed s) *crlf*))

(defun dos2unix (s)
  "Replace all occurences of #\Linefeed by #\Return #\Linefeed"
  (declare (type string s))
  (string-join (split-sequence s *crlf*) #\Linefeed))

(5am:test line-endings
  (let ((unix (force-string #(#\A #\Linefeed #\b)))
        (dos (force-string #(#\A #\Return #\Linefeed #\b))))
    (5am:is (string= dos (unix2dos unix)))
    (5am:is (string= unix (dos2unix dos)))))
