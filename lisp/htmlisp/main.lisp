(defun convert-file (file-path)
  (with-open-file (f file-path :direction :input)
    (loop
      for obj = (read f nil nil)
      unless obj
        do (return all) 
      collect obj into all)
    ))

(defun h-body-convertor (body)
  (loop
    for b in body
    collect (tag-route b)))

(defun h-h1-convertor (h1)
  h1
  )

(defun h-paragraph-convertor (p)
  p)

(defun tag-route (tag-content)
  (ccase (car tag-content)
    (h-body (h-body-convertor (cdr tag-content)))
    (h-h1 (h-h1-convertor (cdr tag-content)))
    ((h-p h-paragraph) (h-paragraph-convertor (cdr tag-content)))))
