(defun main (args)
  ;;:= 1. parse args
  ;;:= 2. check if there is recipt of
  ;;:= 3. run homebrew for package not exist
  ;;:= 4. run macchiato for packages have recipe
  ;;; for example, I am installing sbcl
  (load "recipes/sbcl.lisp") ;; or fasl
  ;;:= todo: need sbcl-recipe class
  (let ((r (make-instance 'sbcl)) ;; args maybe input as class arguments here
        )
    (run r) ;;:= run method of recipe and sbcl-recipr
    ) 
  )

(main)
