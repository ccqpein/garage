;;; the top level commands of espresso
(defpackage #:espresso/commands
  (:use #:CL)
  
  (:import-from #:espresso/libs/command
				#:make-command
				#:*command-output*
				#:*command-error*)
    
  (:export #:*commands*
		   #:*plugins-commands*
		   
		   #:pick-command))

(in-package #:espresso/commands)

(defvar *commands* nil
  "the commands that belong espresso")

(defvar *plugins-commands*
  (list
   (cons espresso/plugin/homebrew:*plugin-name*
		 espresso/plugin/homebrew:*commands*))
  "the bucket that contains plugins commands")

(defun pick-command (argv)
  "pick the command from shell arguments"
  (let ((comm (car argv)))
	(or (find-if (lambda (command)
				   (string= comm
							(espresso/libs/command:command-comm command)))
				 *commands*)

		(loop for plugin-commands in *plugins-commands*
			  for p = (loop for plugin-command in (cdr plugin-commands)
							if (string= comm
										(espresso/libs/command:command-comm plugin-command))
							  return plugin-command)
			  if p return p))
	))

(defun install-recipe (&rest recipes)
  (loop for r in recipes
		for filename-version = (str:split ":" r)
		if (> (length filename-version) 2)
		  do (error "recipes too many \":\" inside")
		else
		  do (espresso/recipes:install
			  (apply #'espresso/recipes:look-up-recipe filename-version)
			  :output *command-output*
			  :error-output *command-error*)))

(defun new-recipe ()
  (format *command-output* "The name of recipe (package name):~%")
  (let ((name (read-line))
		(template-file-path (str:concat espresso/config:*espresso-self-folder* "recipe_template")))
	(with-open-file (f (str:concat espresso/config:*espresso-recipes-folder* name ".lisp")
					   :direction :output
					   :if-exists :error
					   :if-does-not-exist :create)
	  (format f
			  (uiop:read-file-string template-file-path)
			  name
			  name)
	  )))

(defun list-installed ()
  ;;:= next
  )

(defun update-recipe (&rest recipes)
  (loop for r in recipes
		for filename-version = (str:split ":" r)
		if (> (length filename-version) 2)
		  do (error "recipes too many \":\" inside")
		else
		  do (espresso/recipes:update
			  (apply #'espresso/recipes:look-up-recipe filename-version)
			  :output *command-output*
			  :error-output *command-error*)))

(defun upgrade-recipe (&rest recipes)
  (loop for r in recipes
		for filename-version = (str:split ":" r)
		if (> (length filename-version) 2)
		  do (error "recipes too many \":\" inside")
		else
		  do (espresso/recipes:upgrade
			  (apply #'espresso/recipes:look-up-recipe filename-version)
			  :output *command-output*
			  :error-output *command-error*)))

(defun uninstall-recipe (&rest recipes)
  (loop for r in recipes
		for filename-version = (str:split ":" r)
		if (> (length filename-version) 2)
		  do (error "recipes too many \":\" inside")
		else
		  do (espresso/recipes:uninstall
			  (apply #'espresso/recipes:look-up-recipe filename-version)
			  :output *command-output*
			  :error-output *command-error*)))

(setf *commands*
	  (list (make-command :comm "install"
						  :doc "install recipe(s)"
						  :func #'install-recipe)
			(make-command :comm "new"
						  :doc "make new recipe"
						  :func #'new-recipe)
			(make-command :comm "update"
						  :doc "update the reciepts"
						  :func #'update-recipe)
			(make-command :comm "upgrade"
						  :doc "upgrade the reciepts"
						  :func #'upgrade-recipe)
			(make-command :comm "uninstall"
						  :doc "uninstall the reciepts"
						  :func #'uninstall-recipe)))
