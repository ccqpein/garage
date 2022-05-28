(ql:quickload "str")

(defstruct (csv-struct
            (:constructor make-csv (headers value-format-s &rest row-values)))
  headers
  row-values
  value-format-s)

(defun random-format-of-csv (csv)
  "give a csv struct and generate the chaos format string of it"
  (format t "~{~a~^,~}~%" (csv-struct-headers csv))
  (loop for row in (csv-struct-row-values csv)
        do (format t (csv-struct-value-format-s csv) row))
  )

(defun add-whitespaces-around (s)
  (str:pad (+ (length s) (random 10))
           s
           :pad-side :center
           :pad-char " "))

(random-format-of-csv
 (make-csv '("a" "b" "c")
           "~{~s~a~a~}~%"
           '("aa" 1 2) '("bb" 4 5)))

(defun main ()
  (let ((csv-headers '())
        (csv-row ))))
