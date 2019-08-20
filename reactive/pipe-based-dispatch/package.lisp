(defpackage cl-pipeline
  (:use :cl)
  (:export
   #:+stdin+
   #:+stdout+
   #:+stderr+

   #:send
   #:receive
   #:defpipeline

   ;; debug
   #:myname))

(defpackage @
  (:use :cl)
  (:export
   #:defhandle
   #:return-nothing
   #:.loop
   #:.exit-when))