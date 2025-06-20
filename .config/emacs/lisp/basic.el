(setq ad-redefinition-action 'accept)
(setq package-check-signature nil)

;; (setenv "GPG_TTY" "$(tty)")
(require 'epa-file)
(epa-file-enable)
(setenv "GPG_AGENT_INFO" nil)
(setq epg-gpg-program "/run/current-system/sw/bin/gpg")
(setq epg-pinentry-mode 'loopback)
(setq auth-source-debug t)
;; (desktop-save-mode 1)

(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "ASPELL_CONF" "DICPATH" "CFLAGS" "LDFLAGS" "PKG_CONFIG_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize)
  (if (and (fboundp 'native-comp-available-p)
					 (native-comp-available-p))
      (progn
				(message "Native comp is available")
	  ;;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
	  ;;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
	  ;;; Append to path to give priority to values from exec-path-from-shell-initialize.
				(add-to-list 'exec-path (concat invocation-directory (file-name-as-directory "bin")) t)
				(setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
																			 (when (getenv "LIBRARY_PATH")
																				 ":")
					 ;;; This is where Homebrew puts libgccjit libraries.
																			 (car (file-expand-wildcards
																						 (expand-file-name "/opt/homebrew/opt/libgccjit/lib/gcc/*")))))
	  ;;; Only set after LIBRARY_PATH can find gcc libraries.
				(setq comp-deferred-compilation t)
				(setq comp-speed 3))
    (message "Native comp is *not* available")))


(use-package no-littering
	:config
	(no-littering-theme-backups)
	(let ((dir (no-littering-expand-var-file-name "lock-files/")))
		(make-directory dir t)
		(setq lock-file-name-transforms `((".*" ,dir t))))
	(require 'recentf)
	(add-to-list 'recentf-exclude
							 (recentf-expand-file-name no-littering-var-directory))
	(add-to-list 'recentf-exclude
							 (recentf-expand-file-name no-littering-etc-directory))
	(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
	)

(recentf-mode 1)

;; this is merged into Emacs, check back when Emacs 30 is released
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package gcmh
  :vc (:fetcher github :repo emacsmirror/gcmh)
  :config
  (gcmh-mode 1))

(use-package direnv
  :custom
  (direnv-always-show-summary nil)
  :config
  (direnv-mode))

(use-package auth-source
	:ensure nil
	:config
	;; (require 'auth-source-1password)
	;; empty auth-sources
	;; (setq auth-sources nil)
	;; (auth-source-1password-enable)
	;; (add-to-list 'auth-sources 'macos-keychain-internet)
	;; (add-to-list 'auth-sources 'macos-keychain-generic)
	;; (auth-source-pass-enable)
	)

(auth-source-pass-enable)

(use-package ultra-scroll
  :vc (:fetcher github :repo jdtsmith/ultra-scroll)
	:init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(provide 'basic)
