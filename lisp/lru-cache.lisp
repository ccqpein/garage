;;; lru-cache inspired by python version.
(defmacro lru-cache ((&rest keys) (keyword name vars &body body))
  "maybe screw up &rest keyword in lambda list.
and the keys cannot be empty"
  (declare (ignore keyword))
  (let ((table (gensym))
        (val (gensym))
        (call-vars-chain (call-lambda-list
                          (multiple-value-list
                           (parse-lambda-list vars)))))
    `(let ((,table (make-hash-table :test 'equal)))
       (defun ,name ,vars
         (let ((,val (gethash (list ,@keys) ,table)))
           (if ,val (return-from ,name ,val)))
         (labels ((inner-fake ,vars
                    ,@body))
           (let ((result (inner-fake ,@call-vars-chain)))
             (setf (gethash (list ,@keys) ,table) result)
             result))))))

(defun parse-lambda-list (l)
  "parse lambda list of function"
  (let (args optionals keys)
    (do ((flag 'args)
         (l l (cdr l)))
        ((not l) (values
                  (reverse args)
                  (reverse optionals)
                  (reverse keys)))
      (tagbody
         (case (car l)
           ('&optional (setf flag 'optionals) (go end))
           ('&key (setf flag 'keys) (go end))
           ('&rest (setf l (cdr l)) (go end))) ;; jump one element
         
         (case flag
           ('args (push (car l) args))
           ('optionals (push (car l) optionals))
           ('keys (push (car l) keys)))
       end))))

(defun call-lambda-list (lambda-plist)
  "input result which multiple-value-list from parse-lambda-list"
  (let ((args (car lambda-plist))
        (optionals (cadr lambda-plist))
        (keys (caddr lambda-plist))
        (result '()))
    (append result
            args  ;; normal args
            (loop ;; optionals
                  for i in optionals
                  if (eq 'cons (type-of i))
                    collect (car i) into cache
                  else
                    collect i into cache
                  finally (return cache))
            (loop
              with cache = '()
              for i in keys
              if (eq 'cons (type-of i))
                do (push (intern (symbol-name (car i)) "KEYWORD") cache)
                and do (push (car i) cache)
              else
                do (push (intern (symbol-name i) "KEYWORD") cache)
                and do (push i cache)
              finally (return (reverse cache))))))

;;; show how it is work
(pprint
 (macroexpand-1
  '(lru-cache (n)
    (defun fibonacci-3 (n)
      "oh yeah"
      (if (< n 2)
          n
          (+ (fibonacci (1- n)) (fibonacci (- n 2))))))))


(pprint
 (macroexpand-1
  '(lru-cache (a b)
    (defun ex (a b &optional c (d 66) &rest keys &key test (start 0))
      (list a b c d keys test start)))))

;;;;;;;;;;;
;;; So actually, &optional check happen in defun, if it finds d, then give d a value, even it is nil. If not, give 66. d is always exsit inside defun, just have different values be given by defun check
;;; http://www.lispworks.com/documentation/HyperSpec/Body/03_dab.htm
(defun ex (a b &optional c (d 66) &rest keys &key test (start 0))
  (format t "~a~%" (list a b c d keys test start))
  (labels ((inner-fake (a b &optional c (d 66) &rest keys &key test (start 0))
             (list a b c d keys test start)))
    (INNER-FAKE A B C D :TEST TEST :START START)))
