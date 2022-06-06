(defun convert-file (file-path)
  (with-open-file (f file-path :direction :input)
    (loop
      for obj = (read f nil nil)
      unless obj
        do (return all) 
      collect obj into all)
    ))

(defun h-head-convertot (headers)
  "")

(defun h-body-convertor (body)
  (let ((all-tags-in-body
          (loop
            for b in body
            collect (tag-route b))))
    (print all-tags-in-body)
    (format nil "<body>~{~a~}</body>" all-tags-in-body)))

(defun h-h1-convertor (h1)
  (format nil "<h1>~a</h1>" (car h1))
  )

(defun h-paragraph-convertor (p)
  (format nil "<p>~a</p>" (car p)))

(defun tag-route (tag-content)
  (ccase (car tag-content)
    (h-head (h-head-convertot (cdr tag-content)))
    (h-body (h-body-convertor (cdr tag-content)))
    (h-h1 (h-h1-convertor (cdr tag-content)))
    ((h-p h-paragraph) (h-paragraph-convertor (cdr tag-content)))))

(defparameter *demo* nil)

(setf *demo* (convert-file "/Users/ccQ/Code/garage/lisp/htmlisp/demo-page.lisp"))
