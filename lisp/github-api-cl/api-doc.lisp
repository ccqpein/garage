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

   (method
    ;;:initarg :method
    :type string
    :accessor method)

   ;; ((slot-name . val)...)
   (slots
    :type list
    :accessor slots)

   (fmt-control
    :type string
    :accessor control-str)))

(defmethod initialize-instance :after ((api api-doc) &key)
  (let ((api-detail (parse-api (api api))))
    ;; update method
    (setf (method api) (car api-detail))

    ;;:= TODO: update slots
    
    )
  )

(defun parse-api (str)
  (declare (string str))
  (let* ((a (str:split " " str))
         (method (car a))
         ;; split api url
         (b (str:split "/" (cadr a)))
         (fmt-control '())
         (slots '()))
    
    (dolist (x b)
      (unless (string= "" x)
        (push "/" fmt-control)
        (if (str:starts-with? ":" x)
            (progn (push "~a" fmt-control)
                   (push x slots))
            (push x fmt-control))))

    (append
     (list method
           (str:join "" (reverse fmt-control)))
     (reverse slots))
    ))
