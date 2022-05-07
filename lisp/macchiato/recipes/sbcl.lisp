;;; sbcl recipes for installing sbcl on mac
(defclass sbcl ()
  ((name :initform "sbcl")
   (version :initform "2.2.4")
   (url :initform "https://downloads.sourceforge.net/project/sbcl/sbcl/2.2.4/sbcl-2.2.4-source.tar.bz2")))

(defmethod run ((r sbcl))
  )

