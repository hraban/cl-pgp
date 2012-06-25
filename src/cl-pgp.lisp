(in-package :cl-pgp)

;; Unit tests

(5am:def-suite cl-pgp)
(5am:in-suite cl-pgp)

(defparameter *tags*
  '((1 . public-session-key)
    (2 . signature)
    (3 . symmetric-session-key)
    (4 . one-pass-signature)
    (5 . secret-key)
    (6 . public-key)
    (7 . secret-subkey)
    (8 . compressed-data)
    (9 . symmetric-data)
    (10 . marker)
    (11 . literal-data)
    (12 . trust)
    (13 . user-id)
    (14 . public-subkey)
    (17 . user-attribute)
    (18 . symmetric-integr-prot-data)
    (19 . modification-detection)
    (60 . private-60)
    (61 . private-61)
    (62 . private-62)
    (63 . private-63)))

(defparameter *sample-signature*
  (coerce
   #(#\- #\- #\- #\- #\- #\B #\E #\G #\I #\N #\  #\P #\G #\P #\  #\M #\E #\S #\S
     #\A #\G #\E #\- #\- #\- #\- #\- #\Return #\Newline #\V #\e #\r #\s #\i #\o
     #\n #\: #\  #\O #\p #\e #\n #\P #\r #\i #\v #\a #\c #\y #\  #\0 #\. #\9 #\9
     #\Return #\Newline #\Return #\Newline #\y #\D #\g #\B #\O #\2 #\2 #\W #\x #\B
     #\H #\v #\7 #\O #\8 #\X #\7 #\O #\/ #\j #\y #\g #\A #\E #\z #\o #\l #\5 #\6
     #\i #\U #\K #\i #\X #\m #\V #\+ #\X #\m #\p #\C #\t #\m #\p #\q #\Q #\U #\K
     #\i #\Q #\r #\F #\q #\c #\l #\F #\q #\U #\D #\B #\o #\v #\z #\S #\Return
     #\Newline #\v #\B #\S #\F #\j #\N #\S #\i #\V #\H #\s #\u #\A #\A #\= #\=
     #\Return #\Newline #\= #\n #\j #\U #\N #\Return #\Newline #\- #\- #\- #\- #\-
     #\E #\N #\D #\  #\P #\G #\P #\  #\M #\E #\S #\S #\A #\G #\E #\- #\- #\- #\-
     #\- #\Return #\Newline)
   'string))

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

(defun decode-armor-header-line (armor-header-line)
  "Extract armor header line text from a header line"
  (declare (type string armor-header-line))
  (unless (surrounded-by-5-dashes-p armor-header-line)
    (error "Header line corrupt"))
  (let ((len (length armor-header-line)))
    (string-trim-whitespace (subseq armor-header-line 5 (- len 5)))))

(5am:test decode-armor-header-line
  (5am:is (string= "BEGIN PGP MESSAGE"
                   (decode-armor-header-line
                    "----- BEGIN PGP MESSAGE  -----")))
  (5am:signals error (decode-armor-header-line "corrupt header line")))

(defparameter *crlf*
  (the string (coerce #(#\Return #\Newline) 'string)))

(defun split-sequence (raw delimiter &key (count 0) (start 0))
  "Split a sequence by another sequence. Count indicates the maximum number of
  subsequences in the resulting list."
  (if (= count 1)
      (list (subseq raw start))
      (let ((idx (search delimiter raw :start2 start)))
        (if idx
            (cons (subseq raw start idx)
                  (split-string raw
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

(defun split-crlf-lines (raw &rest argv &key &allow-other-keys)
  (declare (type string raw))
  (apply #'split-string raw *crlf* argv))

(defun split-first-empty-line (raw)
  (split-string raw (concatenate 'string *crlf* *crlf*) :count 2))

(defun decode-armor-header (raw-header)
  (apply #'cons
         (mapcar #'string-trim-whitespace
                 (split-sequence raw-header ":" :count 2))))

(5am:test decode-armor-header
  (5am:is (equalp '("foo" . "bar")
                  (decode-armor-header "foo: bar")))
  (5am:signals error (decode-armor-header "illegal")))

(defun decode-armor-headers (raw-headers)
  "Decode raw armor headers into alist"
  (declare (type list raw-headers))
  (mapcar #'decode-armor-header raw-headers))

(defun decode-armor-header-block (raw-header)
  "Decode top segment of armor message into header line and headers"
  (declare (type string raw-header))
  (destructuring-bind (header-line &rest headers) (split-crlf-lines raw-header)
    (list (decode-armor-header-line header-line)
          (decode-armor-headers headers))))

(defgeneric decode-armor (encoded))

(defmethod decode-armor ((ascii string))
  "Decode ASCII armor encoded data. Returns a list with three elements:

  - header-line
  - headers
  - data
  "
  (destructuring-bind (header-block body-block) (split-first-empty-line ascii)
    (destructuring-bind (header-line headers)
        (decode-armor-header-block header-block)
      ; ...
      (list header-line headers NIL))))
