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

(defgeneric decode-armor (encoded))

(defmethod decode-armor ((ascii string))
  "Decode ASCII armor encoded data. Returns a list with three elements:

  - header-line
  - headers
  - data
  "
  ;; ...
  )