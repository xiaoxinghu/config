(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(scroll-bar-mode -1)
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
;; (global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook (lambda() (display-line-numbers-mode 1)))
(setq display-line-numbers-type 'relative)
(setq inhibit-startup-screen t)
(setq backup-by-copying t)
(setq ring-bell-function 'ignore)
(setq-default tab-width 2)
(global-visual-line-mode t)
;; https://emacs.stackexchange.com/questions/41886/html-mail-background-has-same-color-as-text
(setq shr-color-visible-luminance-min 80)

;; set transparency
;; (set-frame-parameter nil 'alpha-background 85) ; Adjust 85 to your desired opacity (0-100)
;; (add-to-list 'default-frame-alist '(alpha-background . 85)) ; Adjust 85 to your desired opacity

(set-frame-parameter (selected-frame) 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))
;; (set-frame-parameter (selected-frame) 'alpha '(55 55))
;; (add-to-list 'default-frame-alist '(alpha 55 55))

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; (setq pixel-scroll-precision-large-scroll-height 5.0)

;; tab-bar
(setq
 tab-bar-close-button-show nil
 tab-bar-show 1
 tab-bar-new-tab-choice "*scratch*"
 )
(tab-bar-mode t)

;; (setq tab-bar-auto-width nil)
;; (setq tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
;; (global-tab-line-mode -1)
;; (tab-line-mode -1)

(let* ((candidates '("JetBrainsMono Nerd Font-17" "Hack Nerd Font-17" "Fira Code-17"))
       (chosen (seq-find (lambda (n) (member n (font-family-list))) candidates "JetBrainsMono Nerd Font-17")))
  (cl-pushnew
   (cons 'font chosen)
   default-frame-alist
   :key #'car :test #'eq))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package mixed-pitch
  :disabled t
  :hook
  ;; If you want it in all text modes:
  ((org-mode markdown-mode) . mixed-pitch-mode))

(use-package doom-modeline
	;; :disabled t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-workspace-name t))
  )

(use-package hl-line ; built in
  :ensure nil
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  ;;; I don't need hl-line showing in other windows. This also offers a small
  ;;; speed boost when buffer is displayed in multiple windows.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

(use-package modus-themes
  :disabled t
  :custom
  (my/dark-theme 'modus-vivendi-tinted)
  (my/light-theme 'modus-operandi-tinted)
  :init
  ;; Always reload the theme for changes to take effect!

  (setq modus-themes-custom-auto-reload nil
        modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-org-blocks nil
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts nil
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  (setq modus-themes-common-palette-overrides
        '((cursor magenta-cooler)
          ;; Make the fringe invisible.
          (fringe unspecified)
          ;; Make line numbers less intense and add a shade of cyan
          ;; for the current line number.
          (fg-line-number-inactive "gray50")
          (fg-line-number-active cyan-cooler)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)
          ;; Make the current line of `hl-line-mode' a fine shade of
          ;; gray (though also see my `lin' package).
          (bg-hl-line bg-dim)
          ;; Make the region have a cyan-green background with no
          ;; specific foreground (use foreground of underlying text).
          ;; "bg-sage" refers to Salvia officinalis, else the common
          ;; sage.
          (bg-region bg-sage)
          (fg-region unspecified)
          ;; Make matching parentheses a shade of magenta.  It
          ;; complements the region nicely.
          (bg-paren-match bg-magenta-intense)
          ;; Make email citations faint and neutral, reducing the
          ;; default four colors to two; make mail headers cyan-blue.
          (mail-cite-0 fg-dim)
          (mail-cite-1 blue-faint)
          (mail-cite-2 fg-dim)
          (mail-cite-3 blue-faint)
          (mail-part cyan-warmer)
          (mail-recipient blue-warmer)
          (mail-subject magenta-cooler)
          (mail-other cyan-warmer)
          ;; Change dates to a set of more subtle combinations.
          (date-deadline magenta-cooler)
          (date-scheduled magenta)
          (date-weekday fg-main)
          (date-event fg-dim)
          (date-now blue-faint)
          ;; Make tags (Org) less colorful and tables look the same as
          ;; the default foreground.
          (prose-done cyan-cooler)
          (prose-tag fg-dim)
          (prose-table fg-main)
          ;; Make headings less colorful (though I never use deeply
          ;; nested headings).
          (fg-heading-2 blue-faint)
          (fg-heading-3 magenta-faint)
          (fg-heading-4 blue-faint)
          (fg-heading-5 magenta-faint)
          (fg-heading-6 blue-faint)
          (fg-heading-7 magenta-faint)
          (fg-heading-8 blue-faint)
          ;; Make the active mode line a fine shade of lavender
          ;; (purple) and tone down the gray of the inactive mode
          ;; lines.
          (bg-mode-line-active bg-lavender)
          (border-mode-line-active bg-lavender)

          (bg-mode-line-inactive bg-dim)
          (border-mode-line-inactive bg-inactive)
          ;; Make the prompts a shade of magenta, to fit in nicely with
          ;; the overall blue-cyan-purple style of the other overrides.
          ;; Add a nuanced background as well.
          (bg-prompt bg-magenta-nuanced)
          (fg-prompt magenta-cooler)
          ;; Tweak some more constructs for stylistic constistency.
          (name blue-warmer)
          (identifier magenta-faint)
          (keybind magenta-cooler)
          (accent-0 magenta-cooler)
          (accent-1 cyan-cooler)
          (accent-2 blue-warmer)
          (accent-3 red-cooler)))

  ;; Make the active mode line have a pseudo 3D effect (this assumes
  ;; you are using the default mode line and not an extra package).
  (custom-set-faces
   '(mode-line ((t :box (:style released-button)))))
  :config
  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi-tinted :no-confirm)
  )

(use-package ef-themes
  ;; :disabled t
  :config
  (setq ef-themes-to-toggle '(ef-frost ef-bio))
  (load-theme 'ef-bio :no-confirm))

(use-package year-1984-theme
  :disabled t
  :config
  ;; (load-theme 'year-1984 t)
	(setq my/light-theme 'year-1984)
	)

(use-package doric-themes
  :disabled t
  :demand t
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-light doric-obsidian))
  (setq doric-themes-to-rotate doric-themes-collection)

  (doric-themes-select 'doric-light)

  ;; ;; To load a random theme instead, use something like one of these:
  ;;
  ;; (doric-themes-load-random)
  ;; (doric-themes-load-random 'light)
  ;; (doric-themes-load-random 'dark)

  ;; ;; For optimal results, also define your preferred font family (or use my `fontaine' package):
  ;;
  ;; (set-face-attribute 'default nil :family "Aporetic Sans Mono" :height 160)
  ;; (set-face-attribute 'variable-pitch nil :family "Aporetic Sans" :height 1.0)
  ;; (set-face-attribute 'fixed-pitch nil :family "Aporetic Sans Mono" :height 1.0)

  :bind
  (("<f5>" . doric-themes-toggle)
   ("C-<f5>" . doric-themes-select)
   ("M-<f5>" . doric-themes-rotate)))

(use-package catppuccin-theme
  :disabled t
  :config
  (setq catppuccin-flavor 'mocha) ;; frappe or 'latte, 'macchiato, or 'mocha
  (load-theme 'catppuccin t))

(use-package gruvbox-theme
	:disabled t
	:config
	(load-theme 'gruvbox-dark-medium t))

(use-package nord-theme
	:disabled t
	:config
	(load-theme 'nord t))

(use-package zenburn-theme
	:disabled t
  :config
  (load-theme 'zenburn t))

(use-package doom-themes
  :disabled t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline nil)
  (load-theme 'doom-zenburn t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package solaire-mode
  :requires doom-themes
  :config
  (solaire-global-mode +1))

(defun my/toggle-theme ()
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (let ((appearance (plist-get (mac-application-state) :appearance)))
    (cond ((equal appearance "NSAppearanceNameAqua")
           (load-theme my/light-theme :no-confirm))
          ((equal appearance "NSAppearanceNameDarkAqua")
           (load-theme my/dark-theme :no-confirm)))))

;; (add-hook 'after-init-hook 'my/toggle-theme)
;; (add-hook 'mac-effective-appearance-change-hook 'my/toggle-theme)

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme my/light-theme t))
    ('dark (load-theme my/dark-theme t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;; scrolling
(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)

(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 20
           :header-line-width 4
           :mode-line-width 8
           :tab-width 4
           :right-divider-width 1
           :scroll-bar-width 8
           :fringe-width 10))
  ;; Subtle mode line for modern appearance
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))
  (spacious-padding-mode 1))

(provide 'ui)
