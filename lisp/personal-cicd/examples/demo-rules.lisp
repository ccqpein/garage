;; if I wanna import plugin feature, I need to defpackage

(set-env '((current-workplace . "./")
       (build-number . 123)))

;; show should like format, also shoule has ability to define
;; output stream
(show-env 'current-workplace)

(show-env)

;; should make -command suffix macro
;; like def-command in lib
(shell-command "rm -rf" (str:concat (get-env 'current-workplace) "build"))
(shell-command "mkdir" (str:concat (get-env 'current-workplace) "build"))

(shell-commands
 (list (list "echo" (get-env 'build-number) "> ./tempfile")
       (list "mv ./tempfile ./build/file")))

(check (file-exist "./build/file"))
(check (not (file-exist "./tempfile")))
