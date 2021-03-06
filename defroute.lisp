;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;; Author: varoun (Varoun P)
;;;; Simple routing library for Hunchentoot.

(in-package #:defroute)
; (declaim #.*optimize-default*)

(defvar *dispatch* '()
  "A list of routes to match against. The order matters - the first route that matches wins. A
  route itself is a list of the form (:http-method (:prefix uri) function) or (:http-method
  (:regex uri) function).")

(defun method-matches-p (method request)
  "True if the http method in the REQUEST, a hunchentoot `request' object, matches the METHOD
  (\keyword)."
  (eql method (request-method request)))

(defun uri-matches-p (uri request)
  "True if the uri in the REQUEST, a hunchentoot `request' object, matches the URI (\alist) of
  the form (:prefix uri) or (:regex uri)."
  (case (first uri)
    (:prefix 
     (let ((mismatch (mismatch (script-name request)
                               (second uri)
                               :test #'char=)))
       (or (null mismatch)
           (>= mismatch (length (second uri))))))
    (:regex
     (let ((scanner (create-scanner (second uri))))
       (scan scanner (script-name request))))))

;;; Hook into the Hunchentoot request dispatch mechanism.

(defclass defroute-acceptor (acceptor)
  ()
  (:documentation "The acceptor for the defroute request routing framework."))

(defclass defroute-ssl-acceptor (defroute-acceptor ssl-acceptor)
  ()
  (:documentation "Add support for SSL connections to the defroute framework."))

(defmethod acceptor-dispatch-request ((acceptor defroute-acceptor) request)
  (loop 
     for (method uri handler) in *dispatch*
     when (and (method-matches-p method request)
               (uri-matches-p uri request))
     return (funcall handler request)
     finally (call-next-method)))

(defmacro add-route (route)
  "Take a route specification of the form (:method :type uri handler) and add to *dispatch*."
  `(destructuring-bind (method match-type uri handler) ',route
     (push (list method (list match-type uri) handler)
           *dispatch*)))

;; CL-USER> (require :defroute)
;; :DEFROUTE
;; NIL
;; CL-USER> (in-package :defroute)
;; #<Package "DEFROUTE">
;; DEFROUTE> (defun say-get (request)
;;             (setf (content-type*) "text/plain")
;;             "You used GET on the resource.")
;; ;Compiler warnings :
;; ;   In SAY-GET: Unused lexical variable REQUEST
;; SAY-GET
;; DEFROUTE> (defun say-post (request)
;;             (setf (content-type*) "text/plain")
;;             "You used POST on the resource.")
;; ;Compiler warnings :
;; ;   In SAY-POST: Unused lexical variable REQUEST
;; SAY-POST
;; DEFROUTE> (add-route (:get :prefix "/v1/someresource" say-get))
;; ((:GET (:PREFIX "/v1/someresource") SAY-GET))
;; DEFROUTE> (add-route (:post :prefix "/v1/someresource" say-post))
;; ((:POST (:PREFIX "/v1/someresource") SAY-POST) (:GET (:PREFIX "/v1/someresource") SAY-GET))
;; DEFROUTE> (start (make-instance 'defroute-acceptor :port 8080))
;; 127.0.0.1 - [2013-07-16 16:43:59] "GET /v1/someresource HTTP/1.1" 200 29 "-" "curl/7.28.1"
;; 127.0.0.1 - [2013-07-16 16:44:16] "POST /v1/someresource HTTP/1.1" 200 30 "-" "curl/7.28.1"
;; 127.0.0.1 - [2013-07-16 16:44:26] "GET /v1/someresource HTTP/1.1" 200 29 "-" "curl/7.28.1"
;; #<DEFROUTE-ACCEPTOR (host *, port 8080)>
;; DEFROUTE> 

;; In a shell
;; $ curl http://localhost:8080/v1/someresource
;; You used GET on the resource.
;; $ curl -d "q=foo" http://localhost:8080/v1/someresource
;; You used POST on the resource.
;; $
