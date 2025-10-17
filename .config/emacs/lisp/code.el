(setq sgml-basic-offset 'tab)

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . js-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ;; ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (c . ("https://github.com/tree-sitter/tree-sitter-c"))
               (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
               (svelte . ("https://github.com/Himujjal/tree-sitter-svelte"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((c-mode . c-ts-mode)
						 (c++-mode . c++-ts-mode)
						 (c-or-c++-mode . c-or-c++-ts-mode)
						 (python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (lua-mode . lua-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  )

;; (use-package treesit-auto
;; 	:after treesit
;; 	:config
;; 	(global-treesit-auto-mode)
;; 	(setq treesit-auto-install 'prompt))

(add-hook 'python-mode-hook
					(lambda ()
						(setq indent-tabs-mode nil)
						;; (setq tab-width 4)
						(setq py-indent-tabs-mode nil)))

(use-package combobulate
  ;; :disabled t
  :vc (:url "https://github.com/mickeynp/combobulate" :rev :newest)
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :hook (
         (prog-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (html-ts-mode . combobulate-mode)
         )
  ;; :load-path ("~/system/emacs/packages/combobulate")
  )

(use-package evil-textobj-tree-sitter
  :config
  (customize-set-variable
   'evil-textobj-tree-sitter-major-mode-language-alist
   (cons '(tsx-ts-mode . "typescript")
         evil-textobj-tree-sitter-major-mode-language-alist))

  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-outer-text-objects-map "v" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; You can also bind multiple items and we will match the first one we can find
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
  ;; Goto start of next function
  (define-key evil-normal-state-map
              (kbd "]f")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer")))

  ;; Goto start of previous function
  (define-key evil-normal-state-map
              (kbd "[f")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))

  ;; Goto end of next function
  (define-key evil-normal-state-map
              (kbd "]F")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))

  ;; Goto end of previous function
  (define-key evil-normal-state-map
              (kbd "[F")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))
  )

(use-package projectile
  :custom
  (projectile-project-search-path '(("~/projects/" . 2)))
  :init
  :config
  (setq projectile-completion-system 'default)

  (defhydra hydra-project (:hint nil)
    "Project"
    ("p" projectile-switch-project "find project" :color blue)
    ("f" projectile-find-file "file file" :color blue)
    ("b" consult-project-buffer "find buffer" :color blue)
    ("t" jump-to-project-todo "project TODO" :color blue)
    ("r" forge-browse-repository "repos" :color blue)
    ("q" nil))

  (evil-define-key nil 'global
    (kbd "M-p") 'projectile-find-file
    (kbd "M-P") 'projectile-switch-project
    (kbd "M-B") 'consult-project-buffer
    )

  (evil-define-key 'normal 'global
    (kbd "<leader>p") 'hydra-project/body)

  (projectile-mode +1))

;; project
;; (require 'project)

;; capture project todo
(defun my/org-capture-project-todo-file ()
  "Return the path to the TODO.org file in the current project's root directory."
  (let ((project (project-current)))
    (if project
        (concat (project-root project) "TODO.org")
      (user-error "Not in a project"))))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("p" "Project TODO" entry (file my/org-capture-project-todo-file)
                 "* TODO %?\n  %i\n  %a")))

(defun jump-to-project-todo ()
  "Jump to the TODO.org file in the current project's root directory, or show an error if it doesn't exist."
  (interactive)
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (todo-file-path (concat project-root "TODO.org")))
    (if (and project-root (file-exists-p todo-file-path))
        (find-file todo-file-path)
      (message "TODO.org file does not exist in the current project."))))

(use-package magit
  :commands (magit-status magit-blame)
  :init
  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-git-executable "/opt/homebrew/bin/git")
	(magit-diff-arguments '("--ignore-all-space"))
  :config
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  )

(defhydra hydra-merge (:color blue :hint nil)
  "
  ^Navigation^           ^Actions^
  ----------------------------------------
  _n_: next conflict     _a_: keep all
  _p_: previous conflict _b_: keep base
  _j_: next line         _m_: keep mine
  _k_: previous line     _r_: resolve
  _C_: combine with next _RET_: keep current
  _E_: ediff             _q_: quit
  _R_: refine
  "
  ("n" smerge-next :color red)
  ("p" smerge-prev :color red)
  ("j" evil-next-line :color red)
  ("k" evil-previous-line :color red)
  ("C" smerge-combine-with-next)
  ("E" smerge-ediff)
  ("R" smerge-refine)
  ("a" smerge-keep-all)
  ("b" smerge-keep-base)
  ("m" smerge-keep-mine)
  ("r" smerge-resolve)
  ("RET" smerge-keep-current)
  ("q" nil :color blue))

(evil-define-key 'normal smerge-mode-map (kbd "SPC m") 'hydra-merge/body)


(use-package git-gutter
  :after magit
  :init
  (global-git-gutter-mode +1))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; it's slow: https://github.com/dandavison/magit-delta/issues/9
;; (use-package magit-delta
;;   :after magit
;;   :hook (magit-mode . magit-delta-mode))
;; (setq magit-refresh-status-buffer nil)

(use-package browse-at-remote
  :after magit
  :config
  (let ((keymaps (list dired-mode-map magit-log-mode-map magit-status-mode-map)))
    (dolist (keymap keymaps)
      (evil-define-key 'normal keymap (kbd "gb") 'browse-at-remote)))
  )

(use-package forge
  :after magit
  :config
  (kbd "<leader>pr") '("browse repo" . forge-browse-repository))

;; (use-package consult-gh
;;   :after consult
;;   :config
;;   (setq consult-gh-default-orgs-list '("xiaoxinghu" "orgapp" "nib-group"))
;;   (setq consult-gh-default-clone-directory "~/Projects"))

(defhydra hydra-git (:hint nil)
  "git"
  ("g" magit-status "status" :color blue)
  ("b" browse-at-remote "browse" :color blue)
  ("s" magit-stage-buffer-file "stage" :color blue)
  ("S" consult-gh-search-repos "stage" :color blue)
  ("c" magit-commit "commit" :color blue)
  ("p" magit-push "push" :color blue)
  ("l" magit-log "log" :color blue)
  ("f" magit-log-buffer-file "log" :color blue)
  ;; ("b" magit-blame "blame" :color blue)
  ("q" nil "quit"))

(evil-define-key 'normal 'global (kbd "SPC g") 'hydra-git/body)

(use-package apheleia
  :config
	(dolist (mode '(css-mode
                  css-ts-mode
                  js-json-mode
                  js-mode
                  json-mode
                  json-ts-mode
                  js-ts-mode
                  tsx-ts-mode
                  typescript-mode
                  typescript-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) 'biome))

  (apheleia-global-mode +1))

;; Automatically make file executable when =shebang= is found.
(add-hook 'after-save-hook
					'executable-make-buffer-file-executable-if-script-p)

(use-package editorconfig
  :custom
  (editorconfig-exclude-modes '(org-mode))
  :config
  (editorconfig-mode 1))

(with-eval-after-load 'editorconfig
  (add-to-list 'editorconfig-indentation-alist
               '(svelte-ts-mode-indent-offset . svelte-ts-mode-indent-offset)))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'smartparens-mode))

;; Code folding. TBH, I don't fold my code.
(use-package origami
  :config
  (global-origami-mode))

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-idle-delay 0
        eldoc-echo-area-use-multiline-p nil))

;; Better indentation.
(use-package aggressive-indent
	:disabled t
  :config
  (global-aggressive-indent-mode 1))

(use-package eldoc-box
  :disabled t
  :config
  (evil-define-key 'normal 'eglot-mode-map
    "K" 'eldoc-box-help-at-point)
  )


(use-package yasnippet
  :custom
  (yas-snippet-dirs `(,(expand-file-name "snippets" user-emacs-directory)))
  ;; (yas-snippet-dirs '("~/.config/emacs/snippets"))
  :config
  (yas-global-mode 1)
  )

(use-package dockerfile-mode
  :mode "Dockerfile\\'")


;; Major mode for driving just files.
(use-package justl)

(use-package consult-gh
	:after consult)

(provide 'code)
