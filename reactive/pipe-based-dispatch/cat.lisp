(defun cat (&rest args)
  ;; this is actually a 2-state state machine

  ;; 1- inititialization - produces output
  (when args
    (assert (= 1 (length args)))
    (let ((name (car args)))
      (assert (stringp name))
      (let ((str (alexandria:read-file-into-string name)))
        (cl-pipeline:send cl-pipeline:+stdout+ str))))

  ;; 2 - steady-state (noop)
  (lambda ()
    (multiple-value-bind (port-index data)
        (cl-pipeline:receive)
      (declare (ignore port-index))
      (when data
        (cl-pipeline:send cl-pipeline:+stdout+ data)))))
