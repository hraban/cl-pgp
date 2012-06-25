;;;; cl-pgp.asd

(defpackage :cl-pgp.defsystem
  (:use :common-lisp :asdf))

(in-package :cl-pgp.defsystem)

(defsystem #:cl-pgp
  :description "Pure Common Lisp implementation of OpenPGP"
  :version "0.0.0"
  :serial T
  :pathname "src/"
  :depends-on ()
  :components ((:file "package")
               (:file "cl-pgp")))