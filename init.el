;; -*- lexical-binding: t -*-
;; *************************************************************
;; Initialize package sources
;; *************************************************************
(require 'package)

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("org"    . "https://orgmode.org/elpa/")
        ("elpa"   . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; ************************************************************
;; wgrep
;; ************************************************************
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;; ************************************************************
;; Vertico
;; ************************************************************
(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  (setq file-name-shadow-properties '(invisible t intangible t))
  (file-name-shadow-mode +1)
  :init
  (vertico-mode))

;; ************************************************************
;; Orderless
;; ************************************************************
(use-package orderless
  :custom
  (completion-styles '(orderless)))

;; ************************************************************
;; Marginalia
;; ************************************************************
(use-package marginalia
  :init
  (marginalia-mode))

;; ************************************************************
;; Corfu
;; ************************************************************
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary t)
; (corfu-quit-no-match t)

  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous))

  :config
  (setq corfu-auto-delay 0.3)
  :init
  (setq tab-always-indent 'complete)
  (corfu-global-mode))

;; ************************************************************
;; Consult
;; ************************************************************
(use-package consult
  :config
  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)
  (setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args))))

(defun rune-consult-line-evil-history (&rest _)
  "Add latest `consult-line' search pattern to the evil search history ring.
This only works with orderless and for the first component of the search."
  (when (and (bound-and-true-p evil-mode)
             (eq evil-search-module 'evil-search))
    (let ((pattern (car (orderless-pattern-compiler (car consult--line-history)))))
      (add-to-history 'evil-ex-search-history pattern)
      (setq evil-ex-search-pattern (list pattern t t))
      (setq evil-ex-search-direction 'forward)
      (when evil-ex-search-persistent-highlight
        (evil-ex-search-activate-highlight evil-ex-search-pattern)))))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


(advice-add #'consult-line :after #'rune-consult-line-evil-history)

;; ************************************************************
;; Corfu
;; ************************************************************
(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (setq embark-prompter #'embark-completing-read-prompter)
  (setq embark-indicators
	'(embark-minimal-indicator
	  embark-highlight-indicator
	  embark-isearch-highlight-indicator))
 ;(add-to-list 'display-buffer-alist
 ;             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
 ;               nil
 ;               (window-parameters (mode-line-format . none))))
  )

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ************************************************************
;; Which key
;; ************************************************************
(use-package which-key
  :init (which-key-mode))

;; ************************************************************
;; iedit
;; ************************************************************
(use-package iedit)

;; ************************************************************
;; Rainbow Delimiters
;; ************************************************************
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ************************************************************
;; Helpful
;; ************************************************************
(use-package helpful)

;; ************************************************************
;; Evil
;; ************************************************************
(use-package evil
:init
  (setq evil-want-integration t)
; (setq evil-ex-search-persistent-highlight nil)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-fine-undo t)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-move-beyond-eol t)
  (setq evil-search-module 'evil-search)

  :config
  (evil-mode 1)
  (setq evil-shift-width 2)

; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "/") 'consult-line)
  (define-key evil-normal-state-map (kbd "?") 'iedit-mode)
  (define-key evil-normal-state-map (kbd "C-w") 'evil-scroll-line-up)
  (define-key evil-normal-state-map (kbd "P") 'consult-yank-from-kill-ring)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-mc
  :after (evil general)
  :config
  (global-evil-mc-mode 1)
  (setq evil-mc-undo-cursors-on-keyboard-quit t)
  :bind (("C-j" . evil-mc-make-cursor-move-next-line)
         ("C-k" . evil-mc-make-cursor-move-prev-line)))

;; ************************************************************
;; Org
;; ************************************************************
(defun rune/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 0)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . rune/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'org-indent)

;; ************************************************************
;; General
;; ************************************************************
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
   "SPC" '(execute-extended-command :which-key "M-x")
   "."   '(find-file :which-key "find files in current dir")
   "b"   '(:ignore t :which-key "buffer")
   "bb"  '(consult-buffer :which-key "switch buffer")
   "bi"  '(ibuffer :which-key "ibuffer")
   "bk"  '(kill-current-buffer :which-key "kill current buffer")
   "bs"  '(save-buffer :which-key "save-buffer")
   "bS"  '(write-file :which-key "write-file")
   "f"   '(:ignore t :which-key "files")
;  "ff"  '(counsel-fzf :which-key "fzf")
   "fp"  '(projectile-find-file :which-key "find file in project")
   "fr"  '(consult-recent-file :which-key "recently opened files")
   "fo"  '(rune/browse-org :which-key "browse org folder")
   "fi"  '(rune/edit-init :which-key "edit init file")
   "fI"  '(rune/browse-init :which-key "browse init dir")
   "fw"  '(rune/browse-work :which-key "browse work dir")
   "fm"  '(rune/browse-mega :which-key "browse mega")
   "fs"  '(save-buffer :which-key "save-buffer")
   "fS"  '(write-file :which-key "write-file")
   "g"   '(magit :which-key "git")
   "h"   '(:ignore t :which-key "help")
   "ha"  '(apropos :which-key "apropos")
;  "ht"  '(counsel-load-theme :which-key "load theme")
   "hf"  '(helpful-function :which-key "describe function")
   "hv"  '(helpful-variable :which-key "describe variable")
   "hk"  '(helpful-key :which-key "describe key")
   "hF"  '(describe-face :which-key "describe face")
   "j"   '(:ignore t :which-key "jump")
   "ji"  '(consult-imenu :which-key "consult-imenu")
   "o"   '(:package org
           :keymap org-mode-map
           :which-key "org")
   "p"   '(:package projectile
           :keymap projectile-command-map
           :which-key "project")
   "q"   '(:ignore t :which-key "quit")
   "qK"  '(save-buffers-kill-emacs :which-key "save and quit")
   "qq"  '(save-buffers-kill-terminal :which-key "save and quit")
   "s"   '(:ignore t :which-key "search")
   "sd"  '(consult-ripgrep :which-key "consult-ripgrep")
   "w"   '(:package evil
           :keymap evil-window-map
           :which-key "window")
   ))

;; ************************************************************
;; Projectile
;; ************************************************************
(use-package projectile
  :config (projectile-mode))

;; ************************************************************
;; Magit
;; ************************************************************
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; ************************************************************
;; Dired
;; ************************************************************
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :init
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  :config
   (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; ************************************************************
;; Miscellaneous Functions
;; ************************************************************
(defun rune/edit-init ()
  "Edit the init.el file."
  (interactive)
  (find-file "~/build/rune/init.el"))

(defun rune/browse-init ()
  "Browse the init folder."
  (interactive)
  (counsel-find-file "~/build/rune/"))

(defun rune/browse-mega ()
  "Browse Mega."
  (interactive)
  (counsel-find-file "~/mega/"))

(defun rune/browse-work ()
  "Browse the work folder."
  (interactive)
  (counsel-find-file "~/mega/work/"))

(defun rune/browse-org ()
  "Browse the org folder."
  (interactive)
  (counsel-find-file "~/mega/org/"))

(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom/escape (&optional interactive)
  "Run `doom-escape-hook'."
  (interactive (list 'interactive))
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
             (setq this-command 'keyboard-quit))))))

(global-set-key [remap keyboard-quit] #'doom/escape)

(defun +evil-disable-ex-highlights-h ()
      "Disable ex search buffer highlights."
      (when (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-nohighlight)
        t))

  (add-hook 'doom-escape-hook
   #'+evil-disable-ex-highlights-h)

;; ************************************************************
;; Keybindings
;; ************************************************************
(general-define-key "<escape>" 'keyboard-escape-quit)
(general-define-key "C-b" 'counsel-switch-buffer)

;; ************************************************************
;; Doom modeline
;; ************************************************************
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; ************************************************************
;; Doom themes
;; ************************************************************
(use-package doom-themes
  :after doom-modeline
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (setq custom-theme-directory "~/build/rune/themes/")
  (load-theme 'doom-sandstorm t))

;; *************************************************************
;; Splash screen
;; *************************************************************
(add-to-list 'load-path "~/build/emacs-splash")
(require 'splash-screen)

;; ************************************************************
;; Faces
;; ************************************************************

;; ************************************************************
;; Garbage collection
;; ************************************************************
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))
(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))
(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

;; ************************************************************
;; Miscellaneous
;; ************************************************************
(setq make-backup-files nil) ; this stops the annoying ~ files
(require 'recentf)
(recentf-mode 1)
(setq recentf-save-file "~/build/rune/.recentf")
(setq truncate-lines t)

;; ************************************************************
;; Custom
;; ************************************************************
(setq custom-file "~/build/rune/custom.el")
