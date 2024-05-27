(defpackage :jkl-options-test
  (:use :cl)
  (:import-from :fiveam
                :def-suite
                :in-suite
                :test
                :is
                :signals
                :def-fixture
                :with-fixture)
  
  (:import-from
   :jkl-options
   :option1-match-string
   :option2-match-string)
  
  (:export
   :options
   :option1-match
   :option2-match)
  )

(in-package :jkl-options-test)

(def-suite options
  :description "test options")

(def-fixture ignore-case ()
  (let ((*ignore-case* t))
    (&body)))

(def-fixture omit-nulls ()
  (let ((*omit-nulls* t))
    (&body)))

(def-suite option1-match
  :in options
  :description "test string match for option1")

(in-suite option1-match)

(test option1-match-case0
  (is (equal (multiple-value-list (option1-match-string "-K, --config <file> Read config from a file"))
             '("K" "config" "file" "Read config from a file")))
  (is (equal (multiple-value-list (option1-match-string "-a, --append      Append to target file when uploading"))
             '("a" "append" "" "Append to target file when uploading")))
  (is (equal (multiple-value-list (option1-match-string "--capath <dir> CA directory to verify peer against"))
             '("" "capath" "dir" "CA directory to verify peer against"))))

(def-suite option2-match
  :in options
  :description "test string match for option1")

(in-suite option2-match)

(test option2-match-case0
  (is (equal (multiple-value-list (option2-match-string "  -r,  --recursive                 specify recursive download"))
             '("r" "recursive" "" "specify recursive download")))
  
  (is (equal (multiple-value-list (option2-match-string " -l,  --level=NUMBER              maximum recursion depth (inf or 0 for infinite)"))
             '("l" "level" "NUMBER" "maximum recursion depth (inf or 0 for infinite)")))
  
  (is (equal (multiple-value-list (option2-match-string "       --convert-file-only         convert the file part of the URLs only (usually known as the basename)"))
             '("" "convert-file-only" "" "convert the file part of the URLs only (usually known as the basename)")))

  (is (equal (multiple-value-list (option2-match-string "       --warc-max-size=NUMBER      set maximum size of WARC files to NUMBER
"))
             '("" "warc-max-size" "NUMBER" "set maximum size of WARC files to NUMBER"))))
