(defpackage load-thru-git
  (:use :cl))
(in-package :load-thru-git)

(defparameter *GIT-COMMAND* #.(let ((s (make-string-output-stream)))
                                (uiop:run-program '("which" "git") :output s)
                                (string-right-trim '(#\Newline) (get-output-stream-string s))))

(defparameter *OUTPUT-STREAM* t)

(defparameter *REPO-FOLDER*) ;;:= folder for source codes

(defun parse-git-link (link)
  "parse the link to git command arguments" ;;:= need branch and version
  )

(defun git-pull (target)
  (declare (string target))
  (uiop:run-program `(,*GIT-COMMAND* "pull" ,target ,*REPO-FOLDER*) :output *OUTPUT-STREAM*)
  )

;;:= clone to quicklisp local 
(defun git-clone (target))


(defun gitload (git-link)
  (multiple-value-bind (link version branch)
      (parse-git-link git-link)
    
    ))
