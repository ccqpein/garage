(ql:quickload '("woo"))

(load "./vault/telebot.lisp")

(defun run ()
  (woo:run #'listen-update :address "0.0.0.0" :port 80))
