(ql:quickload "yason")

(defparameter *leetcode-all-quiz-url* "https://leetcode.com/api/problems/all/")
(defparameter *leetcode-api* "https://leetcode.com/graphql")
(defparameter *leetcode-token* (car (uiop:read-file-lines "./vault/leetcode-token")))
(defparameter *leetcode-session* (car (uiop:read-file-lines "./vault/leetcode-session")))

(defparameter *leetcode-all-quiz* nil)

(defun fetch-all-quiz-list ()
  (let ((out (make-string-output-stream)))
    (sb-ext:run-program "curl" `(,*leetcode-all-quiz-url*
                                 "-H" ,(format nil "Cookie: csrftoken=~a;LEETCODE_SESSION=~a"
                                               *leetcode-token*
                                               *leetcode-session*)
                                 )
                        :search t
                        :output out
                        :error nil)
    (let* ((response (get-output-stream-string out))
           (json-response (yason:parse response))
           (all-stat-status-pairs (gethash "stat_status_pairs" json-response)))
      (setf *leetcode-all-quiz* all-stat-status-pairs) ;; cache it
      all-stat-status-pairs)))

(defun get-question-title-slug (quiz-stat)
  (let ((stat (gethash "stat" quiz-stat)))
    (gethash "question__title_slug" stat)
    ))

(defun get-question-id (quiz-stat)
  (let ((stat (gethash "stat" quiz-stat)))
    (gethash "frontend_question_id" stat)
    ))

(defun fetch-question-description (title-slug)
  (let ((out (make-string-output-stream)))
    (sb-ext:run-program "curl" `(,*leetcode-api*
                                 "-H" "content-type: application/json"
                                 "-H" ,(format nil "Referer: https//leetcode.com/problems/~a/" title-slug)
                                 "-H" ,(format nil "Cookie: csrftoken=~a;LEETCODE_SESSION=~a"
                                               *leetcode-token*
                                               *leetcode-session*)
                                 "--data-raw"
                                 ,(format nil "{\"operationName\":\"questionData\",\"variables\":{\"titleSlug\":\"~a\"},\"query\":\"query questionData($titleSlug: String!) { question(titleSlug: $titleSlug) { content } }\"}" title-slug))
                        :search t
                        :output out
                        :error nil)
    (let* ((response (get-output-stream-string out))
           (json-response (yason:parse response)))
      (gethash "content" (gethash "question" (gethash "data" json-response))))))

(defun fetch-quiz-description-by-id (id)
  (let ((all-quiz-list (if *leetcode-all-quiz*
                           *leetcode-all-quiz*
                           (fetch-all-quiz-list))))
    (let (q-title-slug)
      (setf q-title-slug (get-question-title-slug
                          (find-if (lambda (q) (= id (get-question-id q)))
                                   all-quiz-list)))
      (fetch-question-description q-title-slug)
      )))
