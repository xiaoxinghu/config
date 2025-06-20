(use-package flycheck
  :disabled t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package jinx
  :ensure nil
  :disabled t
  :config
  (setq jinx-languages "en_GB")
  (define-key evil-normal-state-map (kbd "z =") 'jinx-correct)
  (add-hook 'emacs-startup-hook #'global-jinx-mode)
  )

(provide 'checker)
