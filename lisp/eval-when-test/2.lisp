(eval-when (:load-toplevel)
  (format t ":load-toplevel"))

(eval-when (:compile-toplevel)
  (format t ":compile-toplevel"))

(eval-when (:execute)
  (format t ":execute"))
