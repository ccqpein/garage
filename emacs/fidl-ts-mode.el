;;; eeeeeee
;; (treesit-available-p)
;; (treesit-ready-p 'fidl)
;; (treesit-language-available-p 'fidl)


(require 'treesit)

(defvar fidl-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+   "."      table)
    (modify-syntax-entry ?-   "."      table)
    (modify-syntax-entry ?=   "."      table)
    (modify-syntax-entry ?%   "."      table)
    (modify-syntax-entry ?&   "."      table)
    (modify-syntax-entry ?|   "."      table)
    (modify-syntax-entry ?^   "."      table)
    (modify-syntax-entry ?!   "."      table)
    (modify-syntax-entry ?<   "."      table)
    (modify-syntax-entry ?>   "."      table)
    (modify-syntax-entry ?\\  "\\"     table)
    (modify-syntax-entry ?\'  "\""     table)
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)
    (modify-syntax-entry ?\n  "> b"    table)
    table)
  "Syntax table for `fidl-ts-mode'.")

(defvar fidl-ts-mode--indent-rules
  `((fidl
     ((parent-is "source_file") column-0 0)
     ((parent-is "value_layout") parent-bol 4)
	 ;;((node-is "protocol_member") parent-bol 4)
	 ((node-is "attribute_list") parent-bol 4)
	 
	 ;; ((node-is ,(regexp-opt '("value_layout_member"
	 ;; 						  "struct_layout_member"
	 ;; 						  "ordinal_layout_member"
	 ;; 						  )))
	 ;;  parent-bol 4)
	 ;; ((node-is "declaration_modifiers") parent-bol 0)
	 ((parent-is "protocol_declaration") parent-bol 4)
	 ;;((node-is "protocol_member") parent-bol 4)
	 ;;((node-is "protocol_method") parent-bol 0)

	 
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     (no-node parent-bol 0)
     )))

(defvar fidl-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'fidl
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'fidl
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'fidl
   :feature 'delimiter
   '((["," "." ";" ":"]) @font-lock-delimiter-face)

   ;; :language 'fidl
   ;; :feature 'definition
   ;; '((function_declaration
   ;;    name: (identifier) @font-lock-function-name-face)
   ;;   (method_declaration
   ;;    name: (field_identifier) @font-lock-function-name-face)
   ;;   (method_spec
   ;;    name: (field_identifier) @font-lock-function-name-face)
   ;;   (field_declaration
   ;;    name: (field_identifier) @font-lock-property-name-face)
   ;;   (parameter_declaration
   ;;    name: (identifier) @font-lock-variable-name-face)
   ;;   (short_var_declaration
   ;;    left: (expression_list
   ;;           (identifier) @font-lock-variable-name-face
   ;;           ("," (identifier) @font-lock-variable-name-face)*))
   ;;   (var_spec name: (identifier) @font-lock-variable-name-face
   ;;             ("," name: (identifier) @font-lock-variable-name-face)*))

   :language 'fidl
   :feature 'function
   '((call_expression
      function: (identifier) @font-lock-function-call-face)
     (call_expression
      function: (selector_expression
                 field: (field_identifier) @font-lock-function-call-face)))

   ;; :language 'fidl
   ;; :feature 'keyword
   ;; `([,@go-ts-mode--keywords] @font-lock-keyword-face)

   :language 'fidl
   :feature 'label
   '((label_name) @font-lock-constant-face)

   ;; :language 'fidl
   ;; :feature 'number
   ;; '([(float_literal)
   ;;    (imaginary_literal)
   ;;    (int_literal)] @font-lock-number-face)

   ;; :language 'fidl
   ;; :feature 'string
   ;; '([(interpreted_string_literal)
   ;;    (raw_string_literal)
   ;;    (rune_literal)] @font-lock-string-face)

   ;; :language 'fidl
   ;; :feature 'type
   ;; '([(package_identifier) (type_identifier)] @font-lock-type-face)

   ;; :language 'fidl
   ;; :feature 'property
   ;; '((selector_expression field: (field_identifier) @font-lock-property-use-face)
   ;;   (keyed_element (_ (identifier) @font-lock-property-use-face)))

   ;; :language 'fid
   ;; :feature 'variable
   ;; '((identifier) @font-lock-variable-use-face)

   ;; :language 'fid
   ;; :feature 'escape-sequence
   ;; :override t
   ;; '((escape_sequence) @font-lock-escape-face)

   ;; :language 'fid
   ;; :feature 'error
   ;; :override t
   ;; '((ERROR) @font-lock-warning-face)
   )
  "Tree-sitter font-lock settings for `fidl-ts-mode'.")

(define-derived-mode fidl-ts-mode prog-mode "fidl"
  :group 'fidl
  :syntax-table fidl-ts-mode--syntax-table
  
  (when (treesit-ready-p 'fidl)
    (treesit-parser-create 'fidl)
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "//" (* (syntax whitespace))))

    (setq-local treesit-font-lock-settings fidl-ts-mode--font-lock-settings)

    ;; Indent.
    (setq-local indent-tabs-mode t
                treesit-simple-indent-rules fidl-ts-mode--indent-rules)
    
    ;; Electric
    (setq-local electric-indent-chars
                (append "{}()" electric-indent-chars))
    
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( keyword string type)
                  ( assignment builtin constant decorator
                    escape-sequence number string-interpolation )
                  ( bracket delimiter function operator variable property))))
  
  (treesit-major-mode-setup)
  )

(if (treesit-ready-p 'fidl)
    (add-to-list 'auto-mode-alist '("\\.fidl\\'" . fidl-ts-mode)))

(provide 'fidl-ts-mode)
