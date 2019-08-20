(defun clgrep (&rest pattern)
  "unlike full grep, this reads a pattern from args, then reads stdin and sends matches to stdout"

  ;; this is actually a 2-state state machine

  (cl-pipeline:myname "clgrep")

  ;; 1- inititialization - sets up patter matcher
  (let ((regex nil))
    (when pattern
      (assert (= 1 (length pattern)))
      (setf regex (car pattern)))
    
    ;; 2 - steady-state (read stdin and send matching lines forward)
    (lambda ()
      (multiple-value-bind (port-index data)
          (cl-pipeline:receive)
        (assert (eq cl-pipeline:+stdin+ port-index))
        (let ((successful-match (cl-ppcre:scan regex data)))
          (when successful-match
            (let ((output-str (concatenate 'string "clgrep matched: " data)))
              (cl-pipeline:send cl-pipeline:+stdout+ output-str))))))))
