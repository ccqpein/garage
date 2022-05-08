(in-package #:cl-user)

(defpackage #:macchiato
  (:use #:CL)
  (:import-from #:trivial-download
                #:download)
  (:export #:*macchiato-root-dir*
           #:*download-folder*
           #:extract-tarball
           
           ;; re-export
           #:download
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
  "Extract a tarball (.tar.bz2) file to a directory (*default-pathname-defaults*)."
  (let ((*default-pathname-defaults* target-folder)) ;; rebind 
    (with-open-file (tarball-stream pathname
                                    :direction :input
                                    :element-type '(unsigned-byte 8))
      (archive::extract-files-from-archive
       (archive:open-archive 'archive:tar-archive
                             (chipz:make-decompressing-stream 'chipz:bzip2 tarball-stream)
                             :direction :input)))))
