;; sbcl --noinform --load random-string.lisp 40

(defun random-string (len)
  "65 - 122"
  (loop repeat len
        collect (case (random 3)
                  (0 (random 10))
                  (1 (code-char (+ 65 (random (- 91 65)))))
                  (2 (code-char (+ 97 (random (- 123 97))))))))

(defun main ()
  ;;(format t "~a" sb-ext:*posix-argv*)
  (let ((len (cadr sb-ext:*posix-argv*)))
    (format t "~{~a~}~%" (random-string (parse-integer len))))
  (sb-ext:exit))

(main)
