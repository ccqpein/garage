(defstruct test
  (a "a" :type string))

(defmethod get-a ((input test))
  (test-a input))
