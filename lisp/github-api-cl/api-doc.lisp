;;; github api documents
(defpackage #:github-api-doc
  (:use #:CL)
  )

(in-package #:github-api-doc)

(ql:quickload "str")

(defvar *api-root-url* "https://api.github.com/")

(defvar *api-docs*
  '(
    ("events"
     ("GET /events"
      "GET /repos/:owner/:repo/events"
      "GET /networks/:owner/:repo/events"
      "GET /orgs/:org/events"
      "GET /users/:username/received_events"
      "GET /users/:username/received_events/public"
      "GET /users/:username/events"
      "GET /users/:username/events/public"
      "GET /users/:username/events/orgs/:org"))

    ("feeds"
     ("GET /feeds"))

    ("notifications"
     ("GET /notifications"
      "GET /repos/:owner/:repo/notifications"
      "PUT /notifications"
      "PUT /repos/:owner/:repo/notifications"
      "GET /notifications/threads/:thread_id"
      "PATCH /notifications/threads/:thread_id"
      "GET /notifications/threads/:thread_id/subscription"
      "PUT /notifications/threads/:thread_id/subscription"
      "DELETE /notifications/threads/:thread_id/subscription"))

    ("starring"
     ("GET /repos/:owner/:repo/stargazers"
      "GET /users/:username/starred"
      "GET /user/starred"
      "GET /user/starred/:owner/:repo"
      "PUT /user/starred/:owner/:repo"
      "DELETE /user/starred/:owner/:repo"))

    ("watching"
     ("GET /repos/:owner/:repo/subscribers"
      "GET /users/:username/subscriptions"
      "GET /user/subscriptions"
      "GET /repos/:owner/:repo/subscription"
      "PUT /repos/:owner/:repo/subscription"
      "DELETE /repos/:owner/:repo/subscription"))

    ("check runs"
     ("POST /repos/:owner/:repo/check-runs"
      "PATCH /repos/:owner/:repo/check-runs/:check_run_id"
      "GET /repos/:owner/:repo/commits/:ref/check-runs"
      "GET /repos/:owner/:repo/check-suites/:check_suite_id/check-runs"
      "GET /repos/:owner/:repo/check-runs/:check_run_id"
      "GET /repos/:owner/:repo/check-runs/:check_run_id/annotations"))

    ("check suites"
     ("GET /repos/:owner/:repo/check-suites/:check_suite_id"
      "GET /repos/:owner/:repo/commits/:ref/check-suites"
      "PATCH /repos/:owner/:repo/check-suites/preferences"
      "POST /repos/:owner/:repo/check-suites"
      "POST /repos/:owner/:repo/check-suites/:check_suite_id/rerequest"))

    ("code scanning"
     ("GET /repos/:owner/:repo/code-scanning/alerts"
      "GET /repos/:owner/:repo/code-scanning/alerts/:alert_id"))

    ;;;; stop here
    ))

(defclass api-doc ()
  ((api
    :initarg :api
    :type string
    :accessor api)

   (http-method
    :type string
    :accessor http-method)

   ;; (slot-name...)
   (slots
    :type cons
    :accessor slots)

   (fmt-control
    :type string
    :accessor control-str)))

(defmethod initialize-instance :after ((api api-doc) &key)
  (let ((api-detail (parse-api (api api))))
    ;; update method
    (setf (http-method api) (car api-detail)
          (control-str api) (cadr api-detail))

    (setf (slots api)
          (cddr api-detail))
    ))

(defun parse-api (str)
  "give an api entry, return (method format-control slots)"
  (declare (string str))
  (let* ((a (str:split " " str))
         (http-method (car a))
         ;; split api url
         (b (str:split "/" (cadr a)))
         (fmt-control '())
         (cache '())
         (slots '()))
    
    (dolist (x b)
      (unless (string= "" x)
        (push "/" cache)
        (if (str:starts-with? ":" x)
            (progn (push "~a" cache)
                   (push x slots)
                   (push (str:join "" (reverse cache)) fmt-control)
                   (setf cache '())
                   )
            (push x cache))))

    (push (str:join "" (reverse cache)) fmt-control)
    
    (the (cons string *)
         (append
          (list http-method
                (reverse fmt-control))
          (reverse slots)))
    ))

(defmethod print-object ((api api-doc) stream)
  (format stream
          "api-doc object:
api: ~a,
http method: ~a,
slots: ~a,
fmt-control: ~a"
          (api api) (http-method api) (slots api) (control-str api)
          ))

(defgeneric make-call-url (api &rest args &key &allow-other-keys)
  (:documentation "Return the url of this api http call"))

(defmethod make-call-url ((api api-doc) &key)
  "make the url of this api ask user to input data and return call
url"
  (let ((result (make-string-output-stream)))
    (loop
      for k in (slots api)
      for ss in (control-str api)
      for v = (progn (format t "What's ~a: " k)
                     (read))
      do (format result ss v)
      finally (format result
                      (car (last (control-str api)))))

    (get-output-stream-string result)
    ))

(defmethod make-call-url ((api api-doc) &rest arg &key &allow-other-keys)
  (destructuring-bind
      (&key
         (owner "" owner-p)
         (repo "" repo-p)
         &allow-other-keys)
      args

    (let ((result (make-string-output-stream)))
      (loop
        for k in (slots api)
        for ss in (control-str api)
        for v = (progn (format t "What's ~a: " k)
                       (read))
        do (format result ss v)
        finally (format result
                        (car (last (control-str api)))))

      (get-output-stream-string result)
      )))
