(defun convert-file (file-path)
  (with-open-file (f file-path :direction :input)
    (loop
      for obj = (read f nil nil)
      unless obj
        do (return all) 
      collect obj into all)
    ))

(defun h-head-convertot (headers &rest args)
  "")

(defun h-body-convertor (body &rest args)
  (let ((all-tags-in-body
          (loop
            for b in body
            collect (tag-route b))))
    (print all-tags-in-body)
    (format nil "<body>~{~a~}</body>" all-tags-in-body)))

(defun h-h1-convertor (h1 &rest args)
  (format nil "<h1>~a</h1>" (car h1))
  )

(defun h-paragraph-convertor (p &rest args)
  (format nil "<p>~a</p>" (car p)))

(defun h-div-convertor (d &rest args)
  (destructuring-bind
      (&key
         (class "" class-p)
       &allow-other-keys)
      args
    (if class-p
        (format nil "<div class=~s>~a</div>" class (car d))
        (format nil "<div>~a</div>" (car d))
        )))

(defun tag-route (tag-content)
  (let (tagname args content)
    (if (consp (car tag-content))
        (setf tagname (caar tag-content)
              args (cdar tag-content)
              content (cdr tag-content))
        (setf tagname (car tag-content)
              content (cdr tag-content))
        )
    (ccase tagname
      (h-head (apply #'h-head-convertot content args))
      (h-body (apply #'h-body-convertor content args))
      (h-h1 (apply #'h-h1-convertor content args))
      ((h-p h-paragraph) (apply #'h-paragraph-convertor content args))
      (h-div (apply #'h-div-convertor content args))
      )))

(defparameter *demo* nil)

(setf *demo* (convert-file "/Users/ccQ/Code/garage/lisp/htmlisp/demo-page.lisp"))
