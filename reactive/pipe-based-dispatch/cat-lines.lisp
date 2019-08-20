(defun cat-lines (&rest args)
  ;; sends file line-by-line all at once
  ;; this is a poor solution, since it can overflow the downstream input queue
  
  ;; a better solution is to run a request-send protocol - make some (other) port the request input
  ;; send exactly one line every time the request port is tickled

  ;; this is actually a 2-state state machine

  (cl-pipeline:myname "cat-lines")

  ;; 1- inititialization - produces output
  (let ((str-list nil))
    (labels ((send-all-lines ()
               (@:.loop
                (@:.exit-when (null str-list))
                (cl-pipeline:send cl-pipeline:+stdout+ (pop str-list)))))

      (when args
        (assert (= 1 (length args)))
        (let ((name (car args)))
          (assert (stringp name))
          (setf str-list (cl-ppcre:split #\newline (alexandria:read-file-into-string name)))
          (send-all-lines)))
      
      ;; 2 - steady-state
      (lambda ()
        (multiple-value-bind (port-index data)
            (cl-pipeline:receive)
          (assert (eq cl-pipeline:+stdin+ port-index))
          ;(format *error-output* "cat-lines lambda: ~S ~S~%" port-index data)
          (when (and data (stringp data))
            (setf str-list (cl-ppcre:split #\newline (alexandria:read-file-into-string data)))
              (send-all-lines)))))))
