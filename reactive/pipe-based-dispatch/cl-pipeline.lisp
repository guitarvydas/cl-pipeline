(in-package :cl-pipeline)

(defconstant +stdin+  0)
(defconstant +stdout+ 1)
(defconstant +stderr+ 2)

(defconstant +n-pipes+ 16)

(defstruct pipe
  output input)

;; in Lisp (and languages that can return a continuation / callback), we can combine the "initialization" and 
;; "steady-state" states into one function - the function performs initialization, then returns a closure as
;; the "steady-state" lambda

(defstruct pid
  steady-state-function
  pipe-array)

;; globals are OK, as long as we don't let them escape this file and don't involve them in spaghetti code
(defparameter *current-pid* nil)
(defparameter *pipe-list* nil)
(defparameter *pid-list* nil)

(defmacro discard-first-item (list)
  `(pop ,list))

(defmacro discard-first-pid (pid-list)
  ;; for looping - drop first pid from list
  `(discard-first-item ,pid-list))


;; utilities

(defun pipe-2-pids-together (out-pid in-pid)
  ;; in this demo, we only used +stdout+ and +stdin+, extensions to other FD's left as exercise
  ;; return the new pipe
  (let ((new-pipe (make-pipe)))
    (setf (aref (pid-pipe-array out-pid) +stdout+) new-pipe
          (aref (pid-pipe-array in-pid)  +stdin+)  new-pipe)
    new-pipe))

(defun make-pid-for-command (cmd)
  ;; cmd is of the form (func args) or (func)
  (declare (ignore cmd))
  (let ((pid (make-pid :pipe-array (make-array +n-pipes+ :initial-element nil))))
    pid))

(defun set-steady-state-function-for-pid (pid func)
  (setf (pid-steady-state-function pid) func))
  
(defun set-current-pid (pid)
  ;; A way to know which PID is currently running (fed back into SEND)
  ;; This could be optimized away with a few more pointers in the pipes ... I've left it inefficient for clarity.
  (setf *current-pid* pid))

(defun unset-current-pid ()
  (set *current-pid* nil))

(defun get-current-pid ()
  *current-pid*)

(defun get-current-output-pipe ()
  (let ((pid (get-current-pid)))
    (aref (pid-pipe-array pid) +stdout+)))

(defun find-pids-for-pipe (pipe)
  "return 2 values: output pid, input pid for the given pipe"
  ;; this is left unoptimized for clarity/simplicity
  (let ((out-pid nil)
        (in-pid nil))
    (let ((pid-list *pid-list*))
      (@loop
       (@exit-when (eq (aref (pid-pipe-array (first pid-list)) +stdin+) pipe))
       (pop pid-list))
      (assert (not (null pid-list)))
      (setf in-pid (first pid-list)))
    (let ((pid-list *pid-list*))
      (@loop
       (@exit-when (eq (aref (pid-pipe-array (first pid-list)) +stdout+) pipe))
       (pop pid-list))
      (assert (not (null pid-list)))
      (setf out-pid (first pid-list)))
    (values out-pid in-pid)))

(defun mapc-random (func lis)
  (when lis
    (let ((item (nth (random (length lis)) lis)))
      (funcall func item)
      (mapc-random func (remove item lis)))))

(defun invoke-pid (pid)
  (funcall (pid-steady-state-function pid)))

(defun get-input (pid index)
  (let ((pipe (aref (pid-pipe-array pid) index)))
    (pop (pipe-input pipe))))


;; API

(defmacro defpipeline (&rest pipeline)
  `(progn
     (setf *pid-list* nil
           *pipe-list* nil)
     (@create-pipeline ',pipeline)))

(defun send (output-index data)
  (assert (= +stdout+ output-index)) ;; only stdout supported in this first cut, to keep things simple
  (let ((current-output-pipe (get-current-output-pipe)))
    (if (null current-output-pipe)
        (format (if (= +stdout+ output-index) *standard-output* *error-output*)
                "~&~a" data)
      (push data (pipe-output current-output-pipe)))))

(defun receive ()
  ;; return two values: FD index and data
  ;; in this demo we are only concerned with stdin and stdout, they will be hard-coded
  (let ((current-pid (get-current-pid)))
    (let ((current-event-data (get-input current-pid +stdin+)))
      (values +stdin+ current-event-data))))

;; main architectural elements
;; (my convention: routines that begin with the character '@' are fundamental architectural elements)

(defun @create-pipeline (cmd-list)
  ;; in this first cut, we only deal with pipelines of stdout and stdin (not stderr)
  (let ((pid-list (@make-process-id-for-every-command cmd-list)))
    (setf *pid-list* pid-list)
    (@pipe-together pid-list)
    (@initialize-each-process-and-set-steady-state-function cmd-list pid-list)
    (@start-dispatching)
    (values)))

(defun @make-process-id-for-every-command (cmd-list)
  (mapcar #'(lambda (cmd)
              (make-pid-for-command cmd))
          cmd-list))

(defun @pipe-together (pid-list)
  (@loop
   (@exit-when (< (length pid-list) 2))
   (let ((output-process (first pid-list))
         (input-process (second pid-list)))
     (let ((new-pipe (pipe-2-pids-together output-process input-process)))
       (@extend-pipe-list new-pipe))
     (discard-first-pid pid-list))))

(defun @extend-pipe-list (new-pipe)
  (push new-pipe *pipe-list*))

(defun @initialize-each-process-and-set-steady-state-function (cmd-list pid-list)
  ;; call initializer for each process, then set the steady-state function for each process
  ;; in Common Lisp, we can do this in one fell swoop - the initializer returns a LAMBDA which
  ;; is the zero-arg steady-state-function
  (@loop
   (@exit-when (null cmd-list))
   (let ((cmd (first cmd-list)) ;; cmd is (func args) or (func)
         (pid (first pid-list)))
     (set-current-pid pid)
     (let ((ssfunc (apply (first cmd) (rest cmd))))
       (setf (pid-steady-state-function pid) ssfunc)
       (discard-first-item cmd-list)
       (discard-first-item pid-list)))))

(defun @start-dispatching ()
  (run-dispatcher))

(defun @all-pipe-outputs-empty-p (pipe-list)
  (not (some #'(lambda (p)
                 (not (null (pipe-output p))))
             pipe-list)))

;; Dispatcher

(defun run-dispatcher ()
  ;; this loop guarantees that all processes get a chance to run, before any process runs again.
  ;; Randomness "keeps us honest".  Process must be written in the Concurrent Paradigm, since they
  ;; can't count on dispatch ordering.  Clearly, we could optimize this routine to cdr down the pipe-list
  ;; (and delete randomness) if we knew that the Concurrent Paradigm was being honoured.
  (assert (not (null *pipe-list*)))
  (@loop
   (@exit-when (@all-pipe-outputs-empty-p *pipe-list*))
   (mapc-random #'dispatch-one *pipe-list*)))

(defun dispatch-one (pipe)
  ;; See README.md A2.  We must ensure that no process can run more than once in a dispatch cycle.
  (when (pipe-output pipe)
    (let ((single-event (pop (pipe-output pipe))))
      (push single-event (pipe-input pipe))
      (multiple-value-bind (output-pid input-pid)
          (find-pids-for-pipe pipe)
        (declare (ignore output-pid))
        (invoke-pid input-pid)))))

