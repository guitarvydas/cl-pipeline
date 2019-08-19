(in-package :cl-pipeline)

(defmacro @loop (&body body) `(loop ,@body))
(defmacro @exit-when (test) `(when ,test (return)))
(defmacro @end-loop () nil) ;; nothingness


;; in CL, "return" means to pop back to most recent
;; block.  A block is created by the call to the CL:LOOP macro

