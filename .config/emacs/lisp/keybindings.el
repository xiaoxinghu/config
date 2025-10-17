(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(global-set-key (kbd "M-c") nil)

(use-package evil
  :init
  ;; (setq evil-want-C-u-scroll t)
  :custom
  (evil-want-keybinding nil)
  ;; (evil-want-minibuffer t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
  )

;; (with-eval-after-load 'evil-maps
;;   (define-key evil-motion-state-map (kbd "RET") nil))

(use-package evil-collection
  :after evil
  :init
  (setq forge-add-default-bindings nil)
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :config (evil-commentary-mode 1))

(use-package evil-snipe
	:config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
	(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'normal (kbd ",") t)

(use-package which-key
  :config
  (which-key-mode 1))

(use-package hydra)

(evil-define-key nil 'global
  (kbd "M-o") 'find-file
  (kbd "M-d") 'dired-jump
  (kbd "M-w") 'kill-current-buffer
  (kbd "M-q") 'save-buffers-kill-terminal
  ;; (kbd "M-r") 'consult-recent-file
  (kbd "M-b") 'consult-buffer
  ;; (kbd "M-b") 'switch-to-buffer
  (kbd "M-g") 'magit-status
  (kbd "M-s") 'save-buffer
  (kbd "M-S") 'save-some-buffers
  (kbd "M-v") 'yank
  (kbd "M-a") 'mark-whole-buffer
  (kbd "M-f") 'consult-line
  (kbd "M-F") 'consult-ripgrep
  (kbd "M-t") 'tab-new
  (kbd "M-{") 'tab-previous
  (kbd "M-}") 'tab-next
  (kbd "M-=") 'text-scale-increase
  (kbd "M--") 'text-scale-decrease
  (kbd "M-r") 'revert-buffer-quick
  (kbd "M-0") (lambda () (interactive) (text-scale-set 0))
  (kbd "M-n") 'xah-new-empty-buffer
  (kbd "M-`") 'evil-switch-to-windows-last-buffer
  )

(evil-define-key 'normal 'global
  (kbd "<leader>h") '("help" . (keymap))
  (kbd "<leader>hf") '("help" . describe-function)
  (kbd "<leader>hv") '("help" . describe-variable)
  (kbd "<leader>hk") '("help" . describe-key)
  (kbd "<leader>hc") '("help" . what-cursor-position)
  (kbd "<leader>hh") '("help" . help-for-help))

(defhydra hydra-file (:hint nil)
  "Files"
  ("f" find-file "find file" :color blue)
  ("y" yank-location "yank location" :color blue)
  ("r" consult-recent-file "recent files" :color blue)
  ("c" (find-file (expand-file-name "init.el" user-emacs-directory)) "open config" :color blue)
  ("R" my/reload-config "reload config" :color blue)
  ("q" nil))

(evil-define-key 'normal 'global
  (kbd "<leader>f") 'hydra-file/body)

(defhydra hydra-buffer (:hint nil)
  "
Buffers
"
  ("b" consult-buffer "buffers" :color blue)
  ("l" bufler "list" :color blue)
  ("k" kill-current-buffer "kill buffer" :color blue)
  ("s" save-some-buffers "save all buffers" :color blue)
  ("q" nil))

(evil-define-key 'normal 'global
  (kbd "<leader>b") 'hydra-buffer/body)

(evil-define-key 'normal 'global
  (kbd "<leader>s") '("search" . (keymap))
  (kbd "<leader>sm") '("jump to bookmark" . bookmark-jump)
  (kbd "<leader>sb") '("search buffer" . consult-line)
  (kbd "<leader>sB") '("search all open buffer" . consult-line-multi)
  (kbd "<leader>sp") '("search project" . consult-ripgrep))

(evil-define-key 'normal 'global
  (kbd "<leader>u") '("universal argument" . universal-argument)
  (kbd "<leader>`") '("switch" . evil-switch-to-windows-last-buffer)
  (kbd "<leader>o") '("open" . (keymap))
  (kbd "<leader>x") '("eval" . (keymap))
  (kbd "<leader>xx") '("eval" . elisp-eval-region-or-buffer))

(defhydra hydra-toggle (:hint nil)
  "git"
  ("t" ef-themes-toggle "theme" :color blue)
  ("m" toggle-frame-maximized "max" :color blue)
  ("q" nil "quit"))

(evil-define-key 'normal 'global (kbd "SPC t") 'hydra-toggle/body)

;; --- open ---
(defhydra hydra-open (:hint nil)
  "Open"
  ("o" +macos/reveal-in-finder "reveal in finder" :color blue)
  ("a" +macos-open-with "open with app" :color blue)
  ("q" nil))

(evil-define-key 'normal 'global (kbd "SPC o") '("Open" . hydra-open/body))
(evil-define-key 'normal prog-mode-map (kbd "SPC i b") '("insert shebang" . insert-shebang))

(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom/escape (&optional interactive)
  "Run `doom-escape-hook'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
					;; Run all escape hooks. If any returns non-nil, then stop there.
					((run-hook-with-args-until-success 'doom-escape-hook))
					;; don't abort macros
					((or defining-kbd-macro executing-kbd-macro) nil)
					;; Back to the default
					((unwind-protect (keyboard-quit)
						 (when interactive
							 (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'doom/escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape))

(provide 'keybindings)
