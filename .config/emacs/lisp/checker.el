(use-package flycheck
  :disabled t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flyover
	:after flycheck
	:vc (:url "https://github.com/konrad1977/flyover" :rev :newest)
	:hook (flycheck-mode-hook . flyover-mode)
	)

(use-package jinx
  ;; :ensure nil
  ;; :disabled t
  :config
  (setq jinx-languages "en_GB")
  (define-key evil-normal-state-map (kbd "z =") 'jinx-correct)
  (add-hook 'emacs-startup-hook #'global-jinx-mode)
  )

(provide 'checker)
