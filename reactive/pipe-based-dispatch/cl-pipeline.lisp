(in-package :cl-pipeline)

; ARCHITECTURE constants

(defconstant +stdin+  0)
(defconstant +stdout+ 1)
(defconstant +stderr+ 2)

(defconstant +n-pipes+ 16)

; ARCHITECTURAL data (handles only)

(@:defhandle pipe)

(@:defhandle pid)


; ENGINEERING

;; "imp" ==> implementation

(defstruct imp-pipe
  output input)

;; in Lisp (and languages that can return a continuation / callback), we can combine the "initialization" and 
;; "steady-state" states into one function - the function performs initialization, then returns a closure as
;; the "steady-state" lambda

(defstruct imp-pid
  (name nil) ;; debug
  steady-state-function
  pipe-array)


;; globals are OK, as long as we don't let them escape this file and don't involve them in spaghetti code
(defparameter *current-pid* nil)
(defparameter *pipe-list* nil)
(defparameter *pid-list* nil)

(defmacro .discard-first-item (list)
  `(pop ,list))

(defmacro .discard-first-pid (pid-list)
  ;; for looping - drop first pid from list
  `(pop ,pid-list))


;; utilities

(defun .pipe-2-pids-together (out-pid in-pid)
  ;; in this demo, we only used +stdout+ and +stdin+, extensions to other FD's left as exercise
  ;; return the new pipe
  (let ((new-pipe (make-imp-pipe)))
    (setf (aref (imp-pid-pipe-array out-pid) +stdout+) new-pipe
          (aref (imp-pid-pipe-array in-pid)  +stdin+)  new-pipe)
    new-pipe))

(defun .make-pid-for-command (cmd)
  ;; cmd is of the form (func args) or (func)
  (declare (ignore cmd))
  (let ((pid (make-imp-pid :pipe-array (make-array +n-pipes+ :initial-element nil))))
    pid))

(defun .set-steady-state-function-for-pid (pid func)
  (setf (imp-pid-steady-state-function pid) func))
  
(defun .set-current-pid (pid)
  ;; A way to know which PID is currently running (fed back into SEND)
  ;; This could be optimized away with a few more pointers in the pipes ... I've left it inefficient for clarity.
  (setf *current-pid* pid))

(defun .unset-current-pid ()
  (setf *current-pid* nil))

(defun .get-current-pid ()
  *current-pid*)

(defun .find-pids-for-pipe (pipe)
  "return 2 values: output pid, input pid for the given pipe"
  ;; this is left unoptimized for clarity/simplicity
  (let ((out-pid nil)
        (in-pid nil))
    (let ((pid-list *pid-list*))
      (@loop
       (@exit-when (eq (aref (imp-pid-pipe-array (first pid-list)) +stdin+) pipe))
       (.discard-first-item pid-list))
      (assert (not (null pid-list)))
      (setf in-pid (first pid-list)))
    (let ((pid-list *pid-list*))
      (@loop
       (@exit-when (eq (aref (imp-pid-pipe-array (first pid-list)) +stdout+) pipe))
       (.discard-first-item pid-list))
      (assert (not (null pid-list)))
      (setf out-pid (first pid-list)))
    (values out-pid in-pid)))

(defun .mapc-random (func lis)
  (when lis
    (let ((item (nth (random (length lis)) lis)))
      (funcall func item)
      (.mapc-random func (remove item lis)))))

(defun .invoke-pid (pid)
  (when (imp-pid-name pid)
    (format *error-output* "~&invoking pid ~S~%" (imp-pid-name pid)))
  (funcall (imp-pid-steady-state-function pid)))

;;  pipe queing - nb pipes have only 2 parts - input and output.  
;;  (The concept of stdin/stdout refers to the index in a pid's pipe-array).
(defun .dequeue-event-from-output-of-pipe (pipe)
  (let ((single-event (car (last (imp-pipe-output pipe)))))
    (setf (imp-pipe-output pipe) (remove single-event (imp-pipe-output pipe)))
    single-event))

(defun .enqueue-event-to-output-of-pipe (pipe data)
  (setf (imp-pipe-output pipe)
        (append (imp-pipe-output pipe) (list data))))

(defun .enqueue-event-to-input-of-pipe (pipe data)
  (setf (imp-pipe-input pipe)
        (append (imp-pipe-input pipe) (list data))))

(defun .dequeue-event-from-input-of-pipe (pipe)
  (let ((single-event (car (last (imp-pipe-input pipe)))))
    (setf (imp-pipe-input pipe) (remove single-event (imp-pipe-input pipe)))
    single-event))

;; find a pipe within pid's pipe-array
(defun .get-pipe (pid index)
  "return pipe from pid-pipe-array at index"
  (aref (imp-pid-pipe-array pid) index))

(defun .output-to-terminal (output-index data)
  (format (if (= +stdout+ output-index)
              *standard-output*
            *error-output*)
          "~&~a" data))

(defun .initialize () 
  (setf *pid-list* nil
        *pipe-list* nil))

(defun .set-name-of-pid (str)
  (let ((pid (.get-current-pid)))
    (setf (imp-pid-name pid) str)))

(defun .no-pipe-p (p) 
  "return T if p is not a pipe, i.e. end of chain"
  (null p))

(defun .set-pid-list (pid-list)
  (setf *pid-list* pid-list))

(defun .extend-pipe-list (new-pipe)
  (push new-pipe *pipe-list*))

(defun .exec (cmd)
  (apply (first cmd) (rest cmd)))

(defun .all-pipe-outputs-empty-p ()
  (not (some #'(lambda (p)
                 (not (null (imp-pipe-output p))))
             *pipe-list*)))

(defun .log (str)
  (format *error-output* "~&~a~%" str))

(defun .set-steady-state-function (pid ssfunc)
  (setf (imp-pid-steady-state-function pid) ssfunc))

(defun .pipe-list-not-empty ()
  (not (null *pipe-list*)))

(defun .pipe-output-not-empty (pipe)
  (imp-pipe-output pipe))




;; API

(defmacro defpipeline (&rest pipeline)
  `(progn
     (.initialize)
     (@create-pipeline ',pipeline)))

(defun myname (str)
  (.set-name-of-pid str))

(defun send (output-index data)
  (assert (= +stdout+ output-index)) ;; only stdout supported in this first cut, to keep things simple
  (let ((pid (.get-current-pid)))
    (let ((output-pipe (.get-pipe pid output-index)))
      (if (.no-pipe-p output-pipe)
          ;; end of chain - output to terminal
          (.output-to-terminal output-index data)
        (.enqueue-event-to-output-of-pipe output-pipe data)))))

(defun receive ()
  ;; return two values: FD index and data
  ;; in this demo we are only concerned with stdin and stdout, they will be hard-coded
  (let ((current-pid (.get-current-pid)))
    (let ((pipe (.get-pipe current-pid +stdin+)))
      (let ((event-data (.dequeue-event-from-input-of-pipe pipe)))
        (values +stdin+ event-data)))))

(defun .make-process-id-for-every-command (cmd-list)
  (mapcar #'(lambda (cmd)
              (.make-pid-for-command cmd))
          cmd-list))

; ARCHITECTURE

;; (my convention: routines that begin with the character '@' are fundamental architectural elements)

(defun @create-pipeline (cmd-list)
  ;; in this first cut, we only deal with pipelines of stdout and stdin (not stderr)
  (let ((pid-list (.make-process-id-for-every-command cmd-list)))
    (.set-pid-list pid-list)
    (@pipe-together pid-list)
    (@initialize-each-process-and-set-steady-state-function cmd-list pid-list)
    (@start-dispatching)
    (@:return-nothing)))

(defun @pipe-together (pid-list)
  (@loop
   (@exit-when (< (length pid-list) 2))
   (let ((output-process (first pid-list))
         (input-process (second pid-list)))
     (let ((new-pipe (.pipe-2-pids-together output-process input-process)))
       (.extend-pipe-list new-pipe))
     (.discard-first-pid pid-list))))

(defun @initialize-each-process-and-set-steady-state-function (cmd-list pid-list)
  ;; call initializer for each process, then set the steady-state function for each process
  ;; in Common Lisp, we can do this in one fell swoop - the initializer returns a LAMBDA which
  ;; is the zero-arg steady-state-function
  (@loop
   (@exit-when (null cmd-list))
   (let ((cmd (first cmd-list)) ;; cmd is (func args) or (func)
         (pid (first pid-list)))
     (.set-current-pid pid)
     (let ((ssfunc (.exec cmd)))
       (.set-steady-state-function pid ssfunc)
       (.discard-first-item cmd-list)
       (.discard-first-item pid-list))))
  (.unset-current-pid))

(defun @start-dispatching ()
  (@run-dispatcher))

;; Dispatcher

(defun @run-dispatcher ()
  ;; this loop guarantees that all processes get a chance to run, before any process runs again.
  ;; Randomness "keeps us honest".  Process must be written in the Concurrent Paradigm, since they
  ;; can't count on dispatch ordering.  Clearly, we could optimize this routine to cdr down the pipe-list
  ;; (and delete randomness) if we knew that the Concurrent Paradigm was being honoured.
  (assert (.pipe-list-not-empty))
  (.log "Dispatching")
  (@loop
   (@exit-when (.all-pipe-outputs-empty-p))
   (.mapc-random #'@dispatch-one *pipe-list*))
  (.log "End dispatching"))

(defun @dispatch-one (pipe)
  ;; See README.md A2.  We must ensure that no process can run more than once in a dispatch cycle.
  (when (.pipe-output-not-empty pipe)
    (multiple-value-bind (output-pid input-pid)
        (.find-pids-for-pipe pipe)
      (declare (ignore output-pid))
      (let ((single-event (.dequeue-event-from-output-of-pipe pipe)))
        (.enqueue-event-to-input-of-pipe pipe single-event)
        (.set-current-pid input-pid)
        (.invoke-pid input-pid)
        (.unset-current-pid)))))

