(load "./lib.lisp")
(compile-file "./lib-update.lisp")

(defun main ()
  (swank:create-server :port 9527
                       :dont-close t
                       ))

(main)
