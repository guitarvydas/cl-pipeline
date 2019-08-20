(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defsystem cl-pipeline (:optimize ((speed 0) (space 0) (safety 3) (debug 3)))
  :members (
            "package"
            "loop-macros"
            "arch-macros"
            "cl-pipeline"
            "cat"
            "cat-lines"
            "echo"
            "clgrep"
            "main"
            )
  :rules ((:compile :all (:requires (:load :previous)))))
