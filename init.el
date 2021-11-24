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

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

;; ************************************************************
;; wgrep
;; ************************************************************
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t))

;; ************************************************************
;; Vertico
;; ************************************************************
(use-package vertico
  :config
  (setq file-name-shadow-properties '(invisible t intangible t))
  (file-name-shadow-mode +1)
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous))
  :init
  (vertico-mode)
  (setq vertico-cycle t
        vertico-count 15))

(use-package vertico-directory
  :load-path "extensions/"
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)))

(use-package savehist
  :init
  (savehist-mode))

;; ************************************************************
;; Orderless
;; ************************************************************
(use-package orderless
  :custom
  (completion-styles '(substring orderless)))

;; ************************************************************
;; Marginalia
;; ************************************************************
(use-package marginalia
  :bind (:map vertico-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; ************************************************************
;; Corfu
;; ************************************************************
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary nil)

  :bind (:map corfu-map
         ("C-TAB" . corfu-next)
         ("S-TAB" . corfu-previous))

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
  (consult-customize consult-buffer :preview-key (kbd "M-/"))

  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)
  (advice-add #'multi-occur :override #'consult-multi-occur)

  (setq consult-narrow-key "<")

  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; enables swiper-isearch like behavior for consult-line
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

(advice-add #'consult-line :after #'rune-consult-line-evil-history)

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-d" . consult-dir)
         ("C-f" . consult-dir-jump-file)))

;; ************************************************************
;; Embark
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
          embark-isearch-highlight-indicator)))

(defun +embark-sudo-edit ()
  (interactive)
  (find-file (concat "/sudo:root@localhost:" 
                     (expand-file-name (read-file-name "Find file as root: ")))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ************************************************************
;; LSP
;; ************************************************************
(use-package flycheck)

(use-package lsp-mode
  :commands lsp
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :config
  (lsp-ui-doc-enable t)
  (setq lsp-ui-doc-delay 3))

;; ************************************************************
;; Which key
;; ************************************************************
(use-package which-key
  :init (which-key-mode))

;; ************************************************************
;; C 
;; ************************************************************
(use-package ccls
  :init
  (setq ccls-initialization-options
        '(:index (:comments 2) :completion (:detailedLabel t)))
  (add-hook 'c-mode-hook #'lsp))

;; ************************************************************
;; Hydra
;; ************************************************************
(use-package hydra
  :init
  (add-to-list 'load-path (concat user-emacs-directory "modules/"))
  :config
  (require 'windmove)

  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))

  (require 'rune-window-nav-hydra))

;; ************************************************************
;; Smooth Scrolling
;; ************************************************************
(use-package smooth-scrolling
  :init
  (require 'smooth-scrolling)
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 1))

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
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-fine-undo t)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-move-beyond-eol t)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-redo)

  :config
  (evil-mode 1)
  (setq evil-shift-width 2)

  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "/") 'consult-line)
  (define-key evil-visual-state-map (kbd "/") 'consult-line)
  (define-key evil-normal-state-map (kbd "?") 'iedit-mode)
  (define-key evil-visual-state-map (kbd "?") 'iedit-mode)
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

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '(?` . ("`" . "'")) evil-surround-pairs-alist))))

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
  (org-bullets-bullet-list '("●")))

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
   "a"   '(embark-act :which-key "embark-act")
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
   "hf"  '(helpful-function :which-key "describe function")
   "hv"  '(helpful-variable :which-key "describe variable")
   "hk"  '(helpful-key :which-key "describe key")
   "hF"  '(describe-face :which-key "describe face")
   "j"   '(:ignore t :which-key "jump")
   "l"   '(:package lsp-mode
           :keymap lsp-mode-map
           :which-key "lsp")
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
   "si"  '(consult-imenu :which-key "consult-imenu")
   "sd"  '(consult-ripgrep :which-key "consult-ripgrep")
   "t"   '(rune/popup-eshell :which-key "eshell")
   "w"   '(:package evil
           :keymap evil-window-map
           :which-key "window")
   "ww"  '(+hydra/window-nav/body :which-key "resize window")))

;; ************************************************************
;; Projectile
;; ************************************************************
(use-package projectile
  :init
  (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile-cache"))
  (setq projectile-known-projects-file (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))
  :config
  (projectile-mode))


;; ************************************************************
;; Smartparens
;; ************************************************************
(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode))

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

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

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
  (setq custom-theme-directory (concat user-emacs-directory "themes/"))
  (load-theme 'doom-sandstorm t))

;; *************************************************************
;; Popper
;; *************************************************************
(use-package popper
  :bind (("C-;"   . popper-toggle-latest)
         ("C-'"   . popper-cycle)
         ("C-/" . popper-toggle-type))
  :init
  (setq popper-mode-line nil)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          eshell-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) 

;; ************************************************************
;; Miscellaneous Functions
;; ************************************************************
(defun rune/edit-init ()
  "Edit the init.el file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

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
    (evil-ex-nohighlight) t))

(add-hook 'doom-escape-hook #'+evil-disable-ex-highlights-h)

;; ************************************************************
;; Keybindings
;; ************************************************************
(general-define-key "<escape>" 'keyboard-escape-quit)
(general-define-key "C-<tab>" 'completion-at-point)

;; ************************************************************
;; Miscellaneous
;; ************************************************************
(use-package emacs
  :init
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold 16777216 ; 16mb
                    gc-cons-percentage 0.1)))
  (defun doom-defer-garbage-collection-h ()
    (setq gc-cons-threshold most-positive-fixnum))
  (defun doom-restore-garbage-collection-h ()
    (run-at-time
     1 nil (lambda () (setq gc-cons-threshold 16777216))))
  (add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
  (add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)
  (setq custom-theme-directory (concat user-emacs-directory "themes/"))

  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)

  (setq completion-ignore-case t)

  (setq byte-compile-warnings nil)
  (setq warning-suppress-log-types '((comp)))
  (setq warning-suppress-types '((comp)))
  (setq enable-recursive-minibuffers t)
  (setq make-backup-files nil) ; this stops the annoying ~ files
  (setq truncate-lines t)
  (setq word-wrap nil)
  (setq-default indent-tabs-mode nil)
  (setq echo-keystrokes 0.01)

  (require 'recentf)
  (setq recentf-save-file (concat user-emacs-directory ".cache/.recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 200)
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list)

  (setq custom-file "/dev/null")

  (setq auth-sources (concat user-emacs-directory ".authinfo"))

  (setq tramp-auto-save-directory (concat user-emacs-directory ".cache/tramp-autosave"))
  (setq backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not
                (let ((method (file-remote-p name 'method)))
                  (when (stringp method)
                    (member method '("su" "sudo"))))))))

  (setq eshell-history-file-name (concat user-emacs-directory ".cache/eshell-history"))
  (setq savehist-file (concat user-emacs-directory ".cache/minibuffer-history"))
  (setq transient-history-file (concat user-emacs-directory ".cache/transient-history"))

  (defun rune/popup-eshell ()
    (interactive)
    (setq eesh (eshell))
    (popper-lower-to-popup eesh))

  (defun delete-file-and-buffer ()
    "Kill the current buffer and delete the file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if filename (if (y-or-n-p (concat "Really delete '" filename "'?"))
                       (progn
                         (delete-file filename)
                         (message "Deleted %s." filename)
                         (kill-buffer)))
        (message "Failed."))))

  (defun rune/disable-linum-mode ()
    (display-line-numbers-mode -1))
  (add-hook 'eshell-mode-hook 'rune/disable-linum-mode)

  ;; (require 'info-look)
  ;; (info-lookup-setup-mode 'symbol 'emacs-lisp-mode)

  )
