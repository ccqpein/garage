(defun sieve-of-eratosthenes (maximum)
  (loop
     with sieve = (make-array (1+ maximum)
                              :element-type 'bit
                              :initial-element 0)
     for candidate from 2 to maximum
     when (zerop (bit sieve candidate))
     collect candidate
     and do (loop for composite from (expt candidate 2) 
               to maximum by candidate
               do (setf (bit sieve composite) 1))))

(defun sieve-odds (maximum)
  "Prime numbers sieve for odd numbers. 
   Returns a list with all the primes that are less than or equal to maximum."
  (loop :with maxi = (ash (1- maximum) -1)
        :with stop = (ash (isqrt maximum) -1)
        :with sieve = (make-array (1+ maxi) :element-type 'bit :initial-element 0)
        :for i :from 1 :to maxi
        :for odd-number = (1+ (ash i 1))
        :when (zerop (sbit sieve i))
          :collect odd-number :into values
        :when (<= i stop)
          :do (loop :for j :from (* i (1+ i) 2) :to maxi :by odd-number
                    :do (setf (sbit sieve j) 1))
        :finally (return (cons 2 values))))

(expt (+ 1 (/ 1 100000)) 100000)

;;(sieve-of-eratosthenes 10000000000)
;;(sieve-odds 10000000000)

;;; too slow
