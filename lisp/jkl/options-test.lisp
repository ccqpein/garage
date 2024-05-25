(defpackage :options-test
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
   :options
   :option1-match-string)
  
  (:export
   :options
   :option1-match
   :option2-match)
  )

(in-package :options-test)

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

(test case0
      (is (equal (multiple-value-list (option1-match-string "-K, --config <file> Read config from a file"))
                 '("K" "config" "file" "Read config from a file")))
      (is (equal (multiple-value-list (option1-match-string "-a, --append      Append to target file when uploading"))
                 '("a" "append" "" "Append to target file when uploading"))))

(def-suite option2-match
  :in options
  :description "test string match for option1")

(in-suite option2-match)
