(use-package gptel
	:config
	(defhydra hydra-ai (:hint nil)
		"ai"
		("c" gptel "chat" :color blue)
		("r" gptel-rewrite "rewrite" :color blue)
		("s" gptel-send "send" :color blue)
		("a" gptel-add "add" :color blue)
		("?" +ai/translate "ask" :color blue)
		("m" gptel-menu "menu" :color blue))
	;; (setq gptel-api-key (password-store-get "openai/api-key"))
	(setq gptel-api-key (auth-source-pick-first-password :host "openai" :user "api-key"))

  (evil-define-key nil 'global
    (kbd "M-i") 'hydra-ai/body)

	(gptel-make-anthropic "Claude"
		;; :stream t
		:key (auth-source-pick-first-password :host "api.anthropic.com"))

	;; (gptel-make-gemini "Gemini"
  ;;   :stream t
  ;;   :key (auth-source-pick-first-password :host "gemini"))

	;; (gptel-make-openai "DeepSeek"
	;; 	:host "api.deepseek.com"
	;; 	:endpoint "/chat/completions"
	;; 	:stream t
	;; 	:key (auth-source-pick-first-password :host "api.deepseek.com"))

  (setq
   gptel-default-mode #'org-mode
   ;; gptel-model 'claude-3-sonnet-20240229
   ;; gptel-model 'deepseek-chat
   ;; gptel-backend "Claude"
	 )

	(setq gptel-directives
				'((rewrite . gptel--rewrite-directive-default)
					(default
					 . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
					(programming
					 . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
					(writing
					 . "You are a large language model and a writing assistant. Respond concisely.")
					(proofread
					 . "You are a writing assistant. Respond concisely.")
					(chat
					 . "You are a large language model and a conversation partner. Respond concisely.")))
	)

;; (defvar gptel-lookup--history nil)

(defun +ai/translate (prompt)
  (interactive (list (read-string "Ask: " nil)))
  (when (string= prompt "") (user-error "A prompt is required."))
  (gptel-request
			prompt
		:system "translate the following text into Chinese, write a one sentence explanation of the text, in Chinese"
		:callback
		(lambda (response info)
			(if (not response)
					(message "gptel-lookup failed with message: %s" (plist-get info :status))
				(with-current-buffer (get-buffer-create "*gptel-lookup*")
					(let ((inhibit-read-only t))
						(erase-buffer)
						(insert response))
					(special-mode)
					(display-buffer (current-buffer)
													`((display-buffer-in-side-window)
														(side . bottom)
														(window-height . ,#'fit-window-to-buffer))))))))

(use-package gptel-quick
  :vc (:url "https://github.com/karthink/gptel-quick" :rev :newest)
	:config
	;; (keymap-set embark-general-map "?" #'gptel-quick)
	(setq gptel-quick-display nil)
	)

(set-face-foreground 'vertical-border "gold")

(use-package chatgpt-shell
  ;; :disabled t
  :custom
	(chatgpt-shell-streaming nil)
  (chatgpt-shell-openai-key
   (lambda ()
     (auth-source-pick-first-password :host "openai" :user "api-key")))
	;; (chatgpt-shell-anthropic-key
  ;;  (lambda ()
	;; 	 (auth-source-pick-first-password :host "api.anthropic.com")))
  (shell-maker-root-path no-littering-var-directory)
	(with-eval-after-load 'chatgpt-shell
		(evil-define-key 'visual 'global (kbd "M-.") 'chatgpt-shell-proofread-region)
		(evil-define-key 'visual 'global (kbd "M-/") 'chatgpt-shell-explain-code)
		(evil-define-key 'visual 'global (kbd "M-RET") 'chatgpt-shell-send-region))
	(evil-define-key 'normal 'global (kbd "<leader> ar") 'chatgpt-shell-proofread-region)
	;; (leader!
	;;   "a" '(nil :which-key "ai")
	;;   "ar" '(chatgpt-shell-proofread-region :which-key "proofread")
	;;   "ap" '(chatgpt-shell-prompt :which-key "prompt")
	;;   "ae" '(chatgpt-shell-explain-code :which-key "explain")
	;;   "a RET" '(chatgpt-shell-send-region :which-key "send")
	;;   "bk" '(kill-this-buffer :which-key "kill buffer"))
	)

(use-package aidermacs
	:disabled t
  :bind (("C-c a" . aidermacs-transient-menu))
	:init
	(add-hook 'aidermacs-before-run-backend-hook
						(lambda ()
							(setenv "ANTHROPIC_API_KEY" (password-store-get "api.anthropic.com"))
							(setenv "GEMINI_API_KEY" (password-store-get "gemini"))
							))
	(setq aidermacs-backend 'vterm)
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet"))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest)
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-install-dir (no-littering-expand-var-file-name "copilot"))
  (copilot-indent-offset-warning-disable t)
	(copilot-max-char-warning-disable t)
  :bind
  (:map copilot-completion-map
				("<right>" . copilot-accept-completion)
				("C-f" . copilot-accept-completion)
				("C-e" . copilot-accept-completion-by-line)
				("M-j" . copilot-next-completion)
				("M-k" . copilot-previous-completion))
  )


;; (use-package claude-code
;;   :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
;;   :config
;;   ;; optional IDE integration with Monet
;;   (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
;;   (monet-mode 1)

;;   (claude-code-mode)
;;   :bind-keymap ("C-c c" . claude-code-command-map)

;;   ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
;;   :bind
;;   (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

;; (use-package claude-code-ide
;; 	:vc (:fetcher github :repo manzaltu/claude-code-ide.el))

;; (use-package claude-code-ide
;;   :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
;;   :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
;;   :config
;; 	(setq claude-code-ide-terminal-backend 'eat)
;;   ;; (claude-code-ide-emacs-tools-setup)
;; 	) ; Optionally enable Emacs MCP tools

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest)
	:config
	(setq monet-diff-tool #'monet-ediff-tool))

(use-package eca
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest))

(provide 'ai)
