;; need install gsl first
(ql:quickload "qgame")

(in-package #:qgame)

(defparameter *quantum-program*
  `((hadamard 2)
    (hadamard 1)
    (u-theta 0 ,(/ pi 4))
    (oracle oracle-tt 2 1 0)
    (hadamard 2)
    (cnot 2 1)
    (hadamard 2)
    (u-theta 2 ,(/ pi 2))
    (u-theta 1 ,(/ pi 2))))

(defparameter *data* '(0 1 0 0))

(defvar *oracle* (execute-quantum-program *quantum-program* 3 *data*))

(multi-qsys-output-probabilities *oracle* '(2 1))

