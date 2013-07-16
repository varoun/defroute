;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; Author: varoun (Varoun P)
;;;; Simple routing library for Hunchentoot.

(in-package :cl-user)

(defpackage #:defroute
  (:use :cl :hunchentoot :cl-ppcre))

