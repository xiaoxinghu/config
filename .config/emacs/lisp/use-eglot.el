(use-package eglot
  :ensure nil
  :hook
  ((
    typescript-mode
    web-mode
    js2-mode
    js-mode
    yaml-mode
    python-mode
    js-ts-mode
    typescript-ts-mode
    yaml-ts-mode
    svelte-mode
    ) . eglot-ensure)
  :custom
  (eglot-confirm-server-initiated-edits nil)
  :config
  (defun my/eglot-organize-imports ()
    (interactive)
    (if (derived-mode-p major-mode #'typescript-ts-base-mode)
        (seq-do
         (lambda (kind) (interactive)
           (ignore-errors
             (eglot-code-actions (buffer-end 0) (buffer-end 1) kind t)))
         ;; https://github.com/typescript-language-server/typescript-language-server#code-actions-on-save
         (list
          "source.addMissingImports.ts"
          "source.fixAll.ts"
          ;; "source.removeUnused.ts"
          "source.addMissingImports.ts"
          "source.removeUnusedImports.ts"
          ;; "source.sortImports.ts"
          ;; "source.organizeImports.ts"
          ))
      (funcall-interactively #'eglot-code-action-organize-imports)))

  (add-to-list
   'eglot-server-programs
   `(astro-mode . ("astro-ls" "--stdio" :initializationOptions (:typescript (:tsdk ,my/typescript-path)))))

  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelteserver" "--stdio")))

  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("remark-language-server" "--stdio")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t :unstable t))

  ;; (add-to-list
  ;;  'eglot-server-programs
  ;;  '((js-mode js-ts-mode tsx-ts-mode (typescript-ts-mode :language-id "typescript") typescript-mode jsx-mode) . (eglot-deno "deno" "lsp"))
  ;;  )

  ;; (add-to-list
  ;;  'eglot-server-programs
  ;;  '((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode jsx-mode) "deno" "lsp")
  ;;  )

  (add-to-list
   'eglot-server-programs
   '((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode jsx-mode)
     "typescript-language-server" "--stdio"
     :initializationOptions
     (:preferences
      (
       ;; https://github.com/microsoft/TypeScript/blob/main/src/server/protocol.ts#L3410-L3539
       :disableSuggestions                                    :json-false     ;; boolean
       :quotePreference                                       "single"        ;; "auto" | "double" | "single"
       :includeCompletionsForModuleExports                    t               ;; boolean
       :includeCompletionsForImportStatements                 t               ;; boolean
       :includeCompletionsWithSnippetText                     t               ;; boolean
       :includeCompletionsWithInsertText                      t               ;; boolean
       :includeAutomaticOptionalChainCompletions              t               ;; boolean
       :includeCompletionsWithClassMemberSnippets             t               ;; boolean
       :includeCompletionsWithObjectLiteralMethodSnippets     t               ;; boolean
       :useLabelDetailsInCompletionEntries                    t               ;; boolean
       :allowIncompleteCompletions                            t               ;; boolean
       :importModuleSpecifierPreference                       "shortest"      ;; "shortest" | "project-relative" | "relative" | "non-relative"
       :importModuleSpecifierEnding                           "minimal"       ;; "auto" | "minimal" | "index" | "js"
       :allowTextChangesInNewFiles                            t               ;; boolean
       ;; :lazyConfiguredProjectsFromExternalProject                          ;; boolean
       :providePrefixAndSuffixTextForRename                   t               ;; boolean
       :provideRefactorNotApplicableReason                    :json-false     ;; boolean
       :allowRenameOfImportPath                               t               ;; boolean
       ;; :includePackageJsonAutoImports                                      ;; "auto" | "on" | "off"
       :jsxAttributeCompletionStyle                           "auto"          ;; "auto" | "braces" | "none"
       :displayPartsForJSDoc                                  t               ;; boolean
       :generateReturnInDocTemplate                           t               ;; boolean
       :includeInlayParameterNameHints                        "none"           ;; "none" | "literals" | "all"
       ;; :includeInlayParameterNameHintsWhenArgumentMatchesName t               ;; boolean
       ;; :includeInlayFunctionParameterTypeHints                t               ;; boolean,
       ;; :includeInlayVariableTypeHints                         t               ;; boolean
       ;; :includeInlayVariableTypeHintsWhenTypeMatchesName      t               ;; boolean
       ;; :includeInlayPropertyDeclarationTypeHints              t               ;; boolean
       ;; :includeInlayFunctionLikeReturnTypeHints               t               ;; boolean
       :includeInlayEnumMemberValueHints                      t               ;; boolean
       ;; :autoImportFileExcludePatterns                                      ;; string[]
       ;; :organizeImportsIgnoreCase                                          ;; "auto" | boolean
       ;; :organizeImportsCollation                                           ;; "ordinal" | "unicode"
       ;; :organizeImportsCollationLocale                                     ;; string
       ;; :organizeImportsNumericCollation                                    ;; boolean
       ;; :organizeImportsAccentCollation                                     ;; boolean
       ;; :organizeImportsCaseFirst                                           ;; "upper" | "lower" | false
       :disableLineTextInReferences                           :json-false)))
   )

  (defhydra hydra-eglot (:hint nil)
    "language"
    ("a" eglot-code-actions "actions" :color blue)
    ("e" flymake-show-buffer-diagnostics "errors" :color blue)
    ("s" consult-imenu "symbols" :color blue)
    ("r" xref-find-references "reference" :color blue)
    ("R" eglot-reconnect "reconnect" :color blue)
    ("o" eglot-code-action-organize-imports "org imports" :color blue)
    ("j" flymake-goto-next-error "next error" :color red)
    ("k" flymake-goto-prev-error "next error" :color red)
    ("q" nil "quit"))

  ;; (general-nmap :keymaps 'eglot-mode-map "gR" 'eglot-rename)
  ;; (leader! :keymaps 'eglot-mode-map "." 'eglot-code-action-quickfix)
  ;; (leader! :keymaps 'eglot-mode-map "l" 'hydra-eglot/body)

  (evil-define-key 'normal 'eglot-mode-map
    "gR" 'eglot-rename
    ;; "." 'eglot-code-action-quickfix
    ";" 'hydra-eglot/body)
  )

(use-package eglot-booster
  :disabled t
  :after eglot
  :vc (:fetcher github :repo jdtsmith/eglot-booster)
  ;; :vc (eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
  :config	(eglot-booster-mode))

(provide 'use-eglot)
