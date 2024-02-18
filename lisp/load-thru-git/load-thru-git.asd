(defsystem "load-thru-git"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "load-thru-git/tests"))))

(defsystem "load-thru-git/tests"
  :author ""
  :license ""
  :depends-on ("load-thru-git"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for load-thru-git"
  :perform (test-op (op c) (symbol-call :rove :run c)))
