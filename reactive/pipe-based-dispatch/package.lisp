(defpackage cl-pipeline
  (:use :cl)
  (:export
   #:+stdin+
   #:+stdout+
   #:+stderr+

   #:send
   #:receive
   #:defpipeline))