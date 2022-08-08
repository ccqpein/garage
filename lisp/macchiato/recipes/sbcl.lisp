;;; sbcl recipes for installing sbcl on mac
(defclass sbcl (macchiato.recipe:recipe)
  ((name :initform "sbcl")
   (version :initform "2.2.4")
   (url :initform "https://downloads.sourceforge.net/project/sbcl/sbcl/2.2.4/sbcl-2.2.4-source.tar.bz2")
   (filename :initform "sbcl-2.2.4-source.tar.bz2")))

(defmethod install ((r sbcl))
  ;; download to temp dir
  (with-slots (url filename) r
    (macchiato:download-if-not-exist
     url
     (str:concat macchiato::*download-folder* filename)))

  
  (let ((unzip-folder (macchiato:extract-tarball ;; unzip
                       (str:concat macchiato::*download-folder* filename))))
    ;; install
    (uiop:with-current-directory (unzip-folder) (macchiato:async-run-program "sh" "make.sh"))
    )

  ;; make link or not  
  )

;;; sh make.sh
;;; INSTALL_ROOT=~/.Macchiato/sbcl/ sh install.sh

(read-line (uiop/launch-program:process-info-output (uiop/launch-program:launch-program '("ls") :output :stream)))
