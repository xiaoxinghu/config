(use-package vterm
  :custom
  (vterm-module-cmake-args "-DCMAKE_PREFIX_PATH=/opt/homebrew")
  )

(use-package eat)

(use-package pdf-tools
	:disabled t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

(setq
 user-mail-address "hi@huxiaoxing.com"
 user-full-name "Xiaoxing Hu")

(use-package mu4e
	:disabled t
	:ensure nil
	:config
	(setq
	 mu4e-maildir (expand-file-name "~/Mail")
	 mu4e-refile-folder "/Archive"
	 mu4e-sent-folder "/Sent"
	 mu4e-drafts-folder "/Drafts"
	 mu4e-trash-folder "/Trash")
	(setq mu4e-attachment-dir (expand-file-name "~/Downloads"))
	(setq mu4e-get-mail-command "mbsync fastmail")
	(setq mu4e-update-interval (* 10 60)) ;; update every 10 minutes
	(setq mu4e-maildir-shortcuts
				'(
					("/Inbox" . ?i)
					("/Drafts" . ?d)
					("/Sent" . ?s)
					))
	(setq
	 smtpmail-servers-requiring-authorization "fastmail"
   message-send-mail-function   'smtpmail-send-it
	 smtpmail-smtp-service 465
	 smtpmail-stream-type 'ssl
   smtpmail-default-smtp-server "smtp.fastmail.com"
   smtpmail-smtp-server         "smtp.fastmail.com")

	(setq mu4e-bookmarks
				'(
					("flag:unread AND NOT flag:trashed" "Unread" ?u)
					("flag:flagged AND NOT flag:trashed" "Flagged" ?f)
					("date:today..now" "Today" ?t)
					("date:7d..now AND NOT flag:trashed" "Last 7 days" ?w)
					)))

(setq mail-user-agent 'mu4e-user-agent)

(use-package org-msg
	:disabled t
	:config
	(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
				org-msg-startup "hidestars indent inlineimages"
				org-msg-greeting-fmt "\nHi%s,\n\n"
				org-msg-greeting-name-limit 3
				org-msg-default-alternatives '((new	. (text html))
																			 (reply-to-html	. (text html))
																			 (reply-to-text	. (text)))
				org-msg-convert-citation t
				org-msg-signature "
 Regards,

 #+begin_signature
 --
 *Xiaoxing Hu*
 #+end_signature")
	(org-msg-mode))

(use-package mu4e-views
	:disabled t
  :vc (:fetcher github :repo lordpretzel/mu4e-views))

(use-package eaf
  :vc (:fetcher github :repo emacs-eaf/emacs-application-framework)
	:disabled t
	:custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser)
	(require 'eaf-pdf-viewer)
	(require 'eaf-browser)
	)

(use-package pass
	:custom
	(auth-source-pass-filename "~/.local/share/password-store")
	:config
	(auth-source-pass-enable))

(use-package password-store)

(provide 'tools)
