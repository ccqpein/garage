(load "../schema-generator.lisp")

;;
(defstruct-with-query-schema hero
  name
  ago
  (super-power "rich"))
