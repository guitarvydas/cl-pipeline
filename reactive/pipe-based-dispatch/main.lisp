(defun main ()
  (cl-pipeline:defpipeline
   (cat "~/quicklisp/local-projects/cl-pipeline/reactive/pipe-based-dispatch/test.txt")
   (cat)))
  
  