(defpackage #:espresso/libs/fs
  (:use #:CL)
  (:export #:if-file-exist)
  )

(in-package #:espresso/libs/fs)

(defun if-file-exist (file-pathname)
  "check if the file/folder is exist or not"
  (declare (pathname file-pathname))
  (uiop:directory-exists-p file-pathname)
  )

(defun copy-all-files-between-folder (from to)
  "copy all files (sub-folders ain't included) from one place to other.
from and to have to be folder"
  (declare (pathname from to))
  (let ((to-folder (pathname (str:concat (namestring to) "/"))))
	
	(if (ensure-directories-exist to-folder)
		(format t "generate the folder: ~a" (namestring to-folder)))
	
	(dolist (f (uiop:directory-files from))
	  (uiop:copy-file f
					  (pathname (str:concat (namestring to-folder) (pathname-name f)))))))
