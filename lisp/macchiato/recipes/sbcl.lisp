;;; sbcl recipes for installing sbcl on mac
(defclass sbcl ()
  ((name :initform "sbcl")
   (version :initform "2.2.4")
   (url :initform "https://downloads.sourceforge.net/project/sbcl/sbcl/2.2.4/sbcl-2.2.4-source.tar.bz2")
   (filename :initform "sbcl-2.2.4-source.tar.bz2")))

(defmethod install ((r sbcl))
  ;; download to temp dir
  (with-slots (url filename) r
    (macchiato:download-if-not-exist
     url
     (str:concat macchiato::*download-folder* "/" filename)))

  ;; unzip
  (let ((*default-pathname-defaults* macchiato::*download-folder*))
    (macchiato:extract-tarball (str:concat macchiato::*download-folder* "/" filename)))

  ;; make link or not
  )
