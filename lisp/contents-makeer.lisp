;;; same stuff as ../../rusty/contents-maker
(ql:quickload '("cl-ppcre" "str"))

(defvar *title-regex* "^(?<head>#+\\s*)(?<content>[^#]*)")

;; for clean the # symbols inside like ```
(defvar *special-block-sym-stack* '())

(defun code-block? (line)
  (if (cl-ppcre:scan "^```\\w*" line)
      'code-block
      nil))

(defun block-sym-stack-top ()
  (car *special-block-sym-stack*))

(defun regex-scan-title (title)
  "give title and return the sharp number and content"
  (let* ((cl-ppcre:*allow-named-registers* t))
    (if (block-sym-stack-top) ;; in code block stack
        (if (code-block? title)
            ;; remove from stack
            (progn
              (pop *special-block-sym-stack*)
              (values nil nil))
            ;; return
            (values nil nil))
        (if (code-block? title)
            (progn
              (push 'code-block *special-block-sym-stack*)
              (values nil nil))
            
            ;; normal parse below
            (let ((result (cadr (multiple-value-list (cl-ppcre:scan-to-strings *title-regex* title)))))
              (if result
                  (let ((sharps (elt result 0))
                        (content (elt result 1)))
                    (values t
                            (list (count #\# sharps)
                                  (string-right-trim " " content))))
                  (values nil nil)))))
    ))

(defun handle-file (filepath)
  (setf *special-block-sym-stack* '())
  (with-open-file (f filepath :direction :input)
    (loop
      for line = (read-line f nil nil)
      unless line
        return nil
      end

      do (multiple-value-bind (yes-or-no this-line)
             (regex-scan-title line)
           ;;(print line)
           (if yes-or-no
               (let ((count (car this-line))
                     (content (cadr this-line)))
                 (format t
                            "~a-[~a](~a)~%"
                            (format nil "~v@{ ~}" count "")
                            content
                            (string-downcase (str:concat "#" (str:replace-all " " "-" content)))
                            ))
               ))
      )))

(defun main ()
  )
