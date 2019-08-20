(defun main ()
  (cl-pipeline:defpipeline
   (echo "~/quicklisp/local-projects/cl-pipeline/reactive/pipe-based-dispatch/test.txt")
   (cat-lines)
   (clgrep "hello")))
