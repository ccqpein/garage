(eval-when (:compile-toplevel)
  (format t "in compiling~%"))

(eval-when (:compile-toplevel :load-toplevel)
  (format t "in compiling and loading~%"))

(eval-when (:execute)
  (format t "in execute~%"))
