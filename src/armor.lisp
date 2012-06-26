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

(defun split-crlf-lines (raw &rest argv &key &allow-other-keys)
  (declare (type string raw))
  (apply #'split-sequence raw *crlf* argv))

(defun split-first-empty-line (raw)
  (split-sequence raw (concatenate 'string *crlf* *crlf*) :count 2))

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

(5am:test decode-armor
  (destructuring-bind (header-line headers body)
      (decode-armor *sample-signature*)
    (5am:is (string= "BEGIN PGP MESSAGE" header-line))
    (5am:is (string= "OpenPrivacy 0.99" (assocdr "Version"
                                                 headers
                                                 :test #'string=)))))
