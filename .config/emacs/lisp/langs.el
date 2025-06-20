(setq js-indent-level 2)

(defun my/web-mode-hook ()
  "Hooks for Web mode."
	(web-mode-use-tabs)
  (setq web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-script-padding 2
        web-mode-code-indent-offset 2))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.svelte\\'"   . svelte-mode)
         ;; ("\\.[t|j]sx\\'"  . jsx-mode)
         )
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-script-padding 2)
	:hook (web-mode . my/web-mode-hook)
  :config
	;; (web-mode-use-tabs)
  ;; https://github.com/emacs-typescript/typescript.el/issues/4#issuecomment-947866123
  (define-derived-mode jsx-mode web-mode "jsx")
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (define-derived-mode svelte-mode web-mode "svelte")
  ;; (setq web-mode-content-types-alist '(("svelte" . "\\.svelte\\'")))
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp--formatting-indent-alist '(jsx-mode . js-indent-level)))
	)

;; (use-package javascript-mode
;;   :ensure nil
;;   :mode (("\\.[m|c]?js\\'" . javascript-mode))
;;   :config
;;   (setq js-indent-level 2))

(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))

(use-package json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :init
  :config
  ;; (map! :after json-mode
  ;;       :map json-mode-map
  ;;       :localleader
  ;;       :desc "Copy path" "p" #'json-mode-show-path
  ;;       "t" #'json-toggle-boolean
  ;;       "d" #'json-mode-kill-path
  ;;       "x" #'json-nullify-sexp
  ;;       "+" #'json-increment-number-at-point
  ;;       "-" #'json-decrement-number-at-point
  ;;       "f" #'json-mode-beautify)
  )

(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . jsonc-mode))

(use-package yaml-mode)

(use-package csv-mode)

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "multimarkdown")
  (unbind-key "M-p" markdown-mode-map))

(use-package mermaid-mode
  :mode "\\.mmd\\'"
  :config
  (setq mermaid-mmdc-location "docker")
  (setq mermaid-flags "run -u 1000 -v /tmp:/tmp ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:9.1.6")
  )

(use-package terraform-mode
  :custom (terraform-indent-level 4))

(use-package verb)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package lua-mode
  :init
  ;; lua-indent-level defaults to 3 otherwise. Madness.
  (setq lua-indent-level 2)
  )

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-lsp-server 'rust-analyzer))

(provide 'langs)
