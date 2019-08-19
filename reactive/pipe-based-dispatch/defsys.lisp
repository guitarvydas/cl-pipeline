(ql:quickload :alexandria)

(defsystem cl-pipeline (:optimize ((speed 0) (space 0) (safety 3) (debug 3)))
  :members (
            "package"
            "loop-macros"
            "cl-pipeline"
            "cat"
            "echo"
            "main"
            )
  :rules ((:compile :all (:requires (:load :previous)))))
