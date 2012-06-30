;;;; This file holds ASCII Armor related functionality (RFC 4880, section 6.2).
;;;; The armor is a US-ASCII (i.e. lowest 7 bits) encoding of OpenPGP data,
;;;; which is raw binary.
;;;;
;;;; Terminology throughout this file:
;;;;
;;;; Armor: the entire, encoded, payload
;;;; Header: a key-value combination, both text
;;;; Head: the encoded block of armor headers
;;;; Body: the encoded data
;;;; Envelope: the ----- lines at the start and end of the armor
;;;; Header line: the text between the envelope dashes
;;;;

(in-package :cl-pgp)

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

(defun split-crlf-lines (raw &rest argv &key &allow-other-keys)
  (declare (type string raw))
  (apply #'split-sequence raw *crlf* argv))

(defun split-first-empty-line (raw)
  (split-sequence raw (concatenate 'string *crlf* *crlf*) :count 2))

(5am:test split-first-empty-line
  (let ((test-string (coerce #(#\a #\Return #\Newline #\Return #\Newline #\b)
                             'string)))
    (5am:is (equalp '("a" "b") (split-first-empty-line test-string)))))

(defun decode-armor-header (raw-header)
  (apply #'cons
         (mapcar #'string-trim-whitespace
                 (split-sequence raw-header ":" :count 2))))

(5am:test decode-armor-header
  (5am:is (equalp '("foo" . "bar")
                  (decode-armor-header "foo: bar")))
  (5am:signals error (decode-armor-header "illegal")))

(defun decode-armor-header-block (raw-header)
  "Decode top segment of armor message into header line and headers"
  (declare (type string raw-header))
  (mapcar #'decode-armor-header
          (remove-if #'string-empty-p (split-crlf-lines raw-header))))

(defun armor-checksum-p (raw-body idx)
  "Decide whether the checksum for the armor body starts at this index"
  (declare (type string raw-body)
           (type integer idx))
  (when (< 0 idx (1- (length raw-body)))
    (let ((x (char raw-body (1- idx)))
          (y (char raw-body idx))
          (z (char raw-body (1+ idx))))
      (and (newlinep x)
           (char= #\= y)
           (not (newlinep z))
           (not (char= #\= z))))))

(5am:test armor-checksum-p
  (let ((raw (format NIL "base64text~C==~C=abCD" #\Linefeed #\Linefeed)))
    (dotimes (i (length raw))
      (if (= i 14) ; Only the last =, at -4, is the checksum start.
          (5am:is-true (armor-checksum-p raw i))
          (5am:is-false (armor-checksum-p raw i))))))

(defun decode-armor-body-block (raw-body)
  (declare (type string raw-body))
  (destructuring-bind (body64 &optional tail)
      (split-sequence:split-sequence-if #'armor-checksum-p raw-body)
    (unless tail
      (error "No checksum found"))
    ; ...
    body64))

(defun strip-envelope (armor)
  (declare (type string armor))
  (let ((parts (remove-if #'string-empty-p
                          (mapcar #'string-trim-whitespace
                                  (split-sequence armor "-----")))))
    (unless (= 3 (length parts))
      (error "Malformed ASCII armor"))
    (destructuring-bind (header-line mid tail-line) parts
      (unless (and (string-starts-with-p header-line "BEGIN ")
                   (string-starts-with-p tail-line "END ")
                   (string= (subseq tail-line 4) (subseq header-line 6)))
        (error "Malformed ASCII armor header or tail line"))
      (values mid (subseq tail-line 4)))))

(defgeneric decode-armor (encoded))

(defmethod decode-armor ((ascii string))
  "Decode ASCII armor encoded data. Returns a list with three elements:

  - header-line
  - headers
  - packet
  "
  (multiple-value-bind (mid header-line) (strip-envelope ascii)
    (destructuring-bind (a &optional b) (split-first-empty-line mid)
      (let ((head (and b a))
            (body (or b a)))
        (list header-line (when head (decode-armor-header-block head)) body)))))

(5am:test decode-armor
  (destructuring-bind (header-line headers body)
      (decode-armor *sample-signature*)
    (declare (ignore body))
    (5am:is (string= "PGP MESSAGE" header-line))
    (5am:is (string= "OpenPrivacy 0.99" (assocdr "Version"
                                                 headers
                                                 :test #'string=)))))
