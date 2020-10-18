(ql:quickload '("github-api-cl" "yason" "str") :silent t)

(defpackage #:check-contribution
  (:use :cl)
  (:import-from #:github-client :api-client :github-api-call)
  (:import-from #:github-api-doc :api-doc)
  (:export #:if-I-commit-today-with-log)
  )
(in-package #:check-contribution)

(defparameter *list-repos-of-me-api*
  (make-instance 'api-doc
                 :api "GET /users/:username/repos"
                 :parameters '(("type" "string") 
                               ("sort" "string") 
                               ("direction" "string")
                               ("per_page" "integer")
                               ("page" "integer"))))

(defparameter *repos-commits*
  (make-instance 'api-doc
                 :api "GET /repos/:owner/:repo/commits"
                 :parameters '(("since" "string")
                               ("per_page" "integer")
                               ("page" "integer"))))

(defun get-all-repos-names (client)
  (loop for repo in (yason:parse
                     (github-api-call
                      client
                      *list-repos-of-me-api*
                      :username "ccqpein"
                      :type "owner"
                      :sort "updated"
                      ))
        collect (gethash "name" repo)))

(defun if-repos-has-commit-since (client repos since)
  (loop
    for repo-name in repos
    for repo-data = (github-api-call
                     client
                     *repos-commits*
                     :owner "ccqpein"
                     :repo repo-name
                     :since since
                     )
    if (not (zerop (length (yason:parse repo-data))))
      do (return t) ;; return if find any
    finally (return nil)))

(defun time-stamp-today ()
  (let* ((univeral-l (multiple-value-list
                      ;; decode the start of today's UTC time
                      (decode-universal-time
                           ;; encode the start of today's time, current timezone
                       (apply #'encode-universal-time 
                              (append '(0 0 0) ;; sec min hour
                                      (subseq (multiple-value-list
                                               (get-decoded-time))
                                              3 6) ;; day month year
                                      ))
                       0)))
         (time-stamp (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
                             (nth 5 univeral-l)
                             (nth 4 univeral-l)
                             (nth 3 univeral-l)
                             (nth 2 univeral-l)
                             (nth 1 univeral-l)
                             (nth 0 univeral-l)))
         )
    (the string time-stamp)))

(defun read-the-damn-log-and-give-timestamp-to-me (&optional (file-path #P"contribution.log"))
  (declare (pathname file-path))
  (with-open-file (s file-path :if-does-not-exist nil)
    (if s
      (loop
        for line-d = (multiple-value-list (read-line s))
        when (cadr line-d) ;; until the last line
          do (return (car (last (str:split " " (car (str:split ": " (car line-d))))))))
      (error "~a not exsit or cannot open" file-path))))

(defun if-I-commit-today-with-log (&key (file-path #P"contribution.log") token-file (out-stream t))
  (let* ((token (if token-file
                    (with-open-file (fs (pathname token-file) :if-does-not-exist :error)
                      (read-line fs))
                    ""))
         (client (make-instance 'api-client :token token))
         (today (time-stamp-today))
         last-time)
    (tagbody
       ;; read last commit time from log
       (setf last-time
             (handler-bind
                 ((error (lambda (c)
                           (format out-stream "Cannot get timestamp from log, ~a~%" c)
                           (go call-github))))
               (read-the-damn-log-and-give-timestamp-to-me file-path)))

       (format out-stream "Find timestamp in log file~%")
       
       (if (string= last-time today)
           (format out-stream "Timestamp is ~a: ~a" today "You have commit today")
           (progn (format out-stream "Timestamp is not match~%") (go call-github))
           )
       
       (go out)

     call-github
       ;; start with creating new file and call github
       (with-open-file (file-stream file-path :direction :output
                                             :if-exists :overwrite
                                             :if-does-not-exist :create)
         (format out-stream "Calling Github~%")
         (if (if-repos-has-commit-since client
                                        (get-all-repos-names client)
                                        today)
             (progn (format file-stream "Timestamp is ~a: ~a" today "You have commit today")
                    (format out-stream "Timestamp is ~a: ~a" today "You have commit today"))
             (format out-stream "Timestamp is ~a: ~a" today "You haven't commit today")))

     out ;; out
       )))

(defun if-I-commit-today (&optional (out-stream t))
  (let* ((client (make-instance 'api-client))
         (time-stamp (time-stamp-today))
         )
    (format out-stream "Timestamp is ~a: " time-stamp)
    (if (if-repos-has-commit-since client
                                   (get-all-repos-names client)
                                   time-stamp)
        (format out-stream "You have commit today")
        (format t "You haven't commit today"))))

(defun main (argv)
  (if (not argv)
      ;;(if-I-commit-today)
      (if-I-commit-today-with-log)
      (if-I-commit-today-with-log :token-file (car argv)))
  (sb-ext:exit)
  )

;;(main (cdr sb-ext:*posix-argv*))
