(defun lao-bei-jing-er ()
  (loop
    with sentence = (concatenate 'list (read-line))
    for letter in sentence
    collect letter into arr
    collect #\U513F into arr
    finally (return (concatenate 'string arr))
    ))

(defun lao-bei-jing-er ()
  (format nil "~{~c儿~}" (concatenate 'list (read-line)) #\U513F))
