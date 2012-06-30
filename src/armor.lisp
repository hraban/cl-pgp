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
  (dos2unix "-----BEGIN PGP MESSAGE-----
Version: OpenPrivacy 0.99

yDgBO22WxBHv7O8X7O/jygAEzol56iUKiXmV+XmpCtmpqQUKiQrFqclFqUDBovzS
vBSFjNSiVHsuAA==
=njUN
-----END PGP MESSAGE-----
"))

(defun split-lines (raw &rest argv &key &allow-other-keys)
  (declare (type string raw))
  (apply #'split-sequence:split-sequence #\Linefeed raw argv))

(defun split-first-empty-line (raw)
  (split-sequence raw (format NIL "~a~:*~a" #\Linefeed) :count 2))

(5am:test split-first-empty-line
  (let ((test-string (dos2unix "foo:
bar

baz")))
    (5am:is (equalp (list (dos2unix "foo:
bar") "baz") (split-first-empty-line test-string)))))

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
          (remove-if #'string-empty-p (split-lines raw-header))))

(defun decode-armor-body-block (raw-body)
  (declare (type string raw-body))
  (let* ((cksum-start (- (length raw-body) 5))
         (data64-end (1- cksum-start))
         (cksum (subseq raw-body cksum-start))
         (data64 (subseq raw-body 0 data64-end))
         (data (cl-base64:base64-string-to-usb8-array data64)))
    ; ...
    data))

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
        (list header-line
              (when head (decode-armor-header-block head))
              (decode-armor-body-block body))))))

(5am:test decode-armor
  (destructuring-bind (header-line headers body)
      (decode-armor *sample-signature*)
    (declare (ignore body))
    (5am:is (string= "PGP MESSAGE" header-line))
    (5am:is (string= "OpenPrivacy 0.99" (assocdr "Version"
                                                 headers
                                                 :test #'string=)))))
