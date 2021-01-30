;; if I wanna import plugin feature, I need to defpackage

;; when env is a function should define env-var
(env '((current-workplace . "./")
       (build-number . 123)))

;; show should like format, also shoule has ability to define
;; output stream
(show-env 'current-workplace)

(show-env)

;; *job-id* should be job-global-scope var
(show *job-id*)

;; should make -command suffix macro
;; like def-command in lib
(shell-command "rm -rf" (str:concat (get-env 'current-workplace) "build"))
(shell-command "mkdir" (str:concat (get-env 'current-workplace) "build"))

(shell-commands
 '(("echo" build-number "> ./tempfile")
   ("mv ./tempfile ./build/file")))

(check (file-exist "./build/file"))
(check (not (file-exist "./tempfile")))
