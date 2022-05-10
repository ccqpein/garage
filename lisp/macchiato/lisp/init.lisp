(in-package #:cl-user)

(defpackage #:macchiato
  (:use #:CL)
  (:import-from #:trivial-download
                #:download)
  (:export #:*macchiato-root-dir*
           #:*download-folder*
           #:extract-tarball
           
           ))

(in-package #:macchiato)

(defvar *macchiato-root-dir* "~/.Macchiato/")

(defvar *download-folder* "~/Library/Caches/Macchiato/")

(uiop/filesystem:ensure-all-directories-exist (list *macchiato-root-dir* *download-folder*))

(defun download-if-exist (url file-location)
  "download if filename hasn't exist"
  (unless (uiop/filesystem:probe-file*
           file-location)
    (download
     url
     file-location)
    ))

;;; https://gist.github.com/fukamachi/6364983
(defun extract-tarball (pathname &optional (target-folder (pathname *download-folder*)))
  "Extract a tarball (.tar.bz2) file to a directory (*default-pathname-defaults*).
And return the extract folder."
  (let ((*default-pathname-defaults* target-folder)) ;; rebind 
    (with-open-file (tarball-stream pathname
                                    :direction :input
                                    :element-type '(unsigned-byte 8))
      (let ((archive (archive:open-archive 'archive:tar-archive
                                           (chipz:make-decompressing-stream 'chipz:bzip2 tarball-stream)
                                           :direction :input))
            location-name ;; the name of folder after extract
            )
        (archive::extract-files-from-archive
         archive
         ;; filter func, but it actually just
         ;; setf the location name with the first entry name
         ;; https://github.com/froydnj/archive/blob/631271c091ed02994bec3980cb288a2cf32c7cdc/archive.lisp#L239
         (lambda (name) 
           (if (not location-name)
               (setf location-name name)
               t)))
        
        (str:concat (namestring target-folder) location-name)
        ))))

(defun async-run-program (&rest comm)
  "run command and print out"
  (let ((stream (uiop/launch-program:process-info-output
                 (uiop/launch-program:launch-program comm :output :stream))))
    (loop for line = (read-line stream nil nil)
          if line
            do (format t "~a~%" line)
          else
            do (return))
    ))
