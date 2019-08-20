(defun echo (&rest args)
  "sends args to stdout"

  ;; this is actually a 2-state statemachine

  ;; 1 - initialization - send args to stdout
  (cl-pipeline:myname "echo")
  (mapc #'(lambda (arg)
            (cl-pipeline:send cl-pipeline:+stdout+ arg))
        args)

  ;; 2 - steady-state - nothing
  (lambda ()))
