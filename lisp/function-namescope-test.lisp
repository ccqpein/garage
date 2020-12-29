(flet ((f () 24))
  (let ((f (lambda () 42)))
    (f)))
;;; => 24

(let ((f (lambda () 42)))
  (flet ((f () 24))
    (f)))
;;; => 24

(flet ((f () 24))
  (let ((f (lambda () 42)))
    (funcall f)))
;; => 42

(flet ((f () 24))
  (let ((f (lambda () 42)))
    (funcall #'f)))
;; => 24
