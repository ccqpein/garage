(defun nth-nest (l coorp)
  (loop for i in coorp
        when (or (< i 0) (> i (length l)))
          return nil
        do (setf l (nth i l))
        finally (return l)))

;; (nth-nest '((1 2) (3 4)) '(1 0)) => 3

;; (defsetf nth-nest (l coorp) (new-value)
;;   (alexandria:with-gensyms (ll indx)
;;     (let ((ll l)
;;           (indx (eval coorp)))
;;       `(setf ,@(loop
;;                  with l = ll
;;                  for i in indx
;;                  do (setf l `(nth ,i ,l))
                     
;;                  finally (return l)
;;                  )
;;              new-value))))

;; (defun set-nth-nest (l coorp v)
;;   (loop for i in coorp
;;         do (setf l (nth i l))
;;         do (print l)
;;         finally (setf l v)))

(defun %set-nth-nest (l coorp v)
  (cond
    ((not coorp) nil) ;; error
    ((= 1 (length coorp)) (setf (nth (car coorp) l) v))
    (t (set-nth-nest (nth (car coorp) l) (cdr coorp) v))))

(defsetf nth-nest %set-nth-nest)

;; (defmacro nth-nest-test (l coorp)
;;   (alexandria:with-gensyms (a b)
;;     (let ((a l)
;;           (b (eval coorp)))
;;       (format t "~a ~a~%" a b)
;;       `(,@(loop
;;            with l = a
;;            for i in b
;;            ;;do (print l)
;;            do (setf l `(nth ,i ,l))
                 
;;            finally (return l)
;;                    ;;collect l
;;            ))))
;;   )
