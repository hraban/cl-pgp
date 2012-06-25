;;;; cl-pgp.asd

(defpackage :cl-pgp.defsystem
  (:use :common-lisp :asdf))

(in-package :cl-pgp.defsystem)

(defsystem #:cl-pgp
  :description "Pure Common Lisp implementation of OpenPGP"
  :version "0.0.0"
  :author "Hraban Luyat <hraban@0brg.net>"
  :licence "LLGPL"
  :serial T
  :pathname "src/"
  :depends-on (#:fiveam)
  :components ((:file "package")
               (:file "utils")
               (:file "cl-pgp")))