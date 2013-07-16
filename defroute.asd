;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; Author: varoun (Varoun P)
;;;; Simple routing library for Hunchentoot.

(in-package :cl-user)

(defpackage #:defroute-asd
  (:use :cl :asdf))

(in-package :defroute-asd)

(defvar *defroute-version* "0.0.1"
  "The current version of the defroute library.")

(defsystem :defroute
    :serial t
    :version #:*defroute-version*
    :description "Defroute is a minimalist routing library for Hunchentoot."
    :depends-on (:cl-ppcre
                 :hunchentoot)
    :components ((:file "packages")
                 (:file "defroute")))
