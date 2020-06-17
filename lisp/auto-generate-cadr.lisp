(defmacro make-caxdxr (sy)
  (let ((split-str (subseq (concatenate 'list (symbol-name sy))
                           1
                           (1- (length (symbol-name sy)))))
        )
    `(defun ,sy (l)
       ,@(loop
           with result = '(l)
           for e in (reverse split-str)
           do (cond ((char= #\B e) ;;; B => a
                     (setf result (list (cons 'car result))))
                    ((char= #\C e) ;;; C => d
                     (setf result (list (cons 'cdr result))))
                    (t (error "error"))
                    )
           finally (return result)))))


(defmacro auto-generate (ll)
  `(progn
     ,@(loop
         for i in ll
         collect `(make-caxdxr ,i))))


;;; three ways to auto generate code

(auto-generate (cbbccr cbcr))

(loop
  for i in '(cbbccr cbcr)
  do (eval `(make-caxdxr ,i)))

(loop
  for sy in '(cbbccr cbcr)
  do (eval (let ((split-str (subseq (concatenate 'list (symbol-name sy))
                                    1
                                    (1- (length (symbol-name sy))))))
             `(defun ,sy (l)
                ,@(loop
                    with result = '(l)
                    for e in (reverse split-str)
                    do (cond ((char= #\B e) ;;; B => a
                              (setf result (list (cons 'car result))))
                             ((char= #\C e) ;;; C => d
                              (setf result (list (cons 'cdr result))))
                             (t (error "error"))
                             )
                    finally (return result))))))
