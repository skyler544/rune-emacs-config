;; -*- lexical-binding: t -*-
;; *************************************************************
;; Initialize package sources
;; *************************************************************
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(setq use-package-enable-imenu-support t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; *************************************************************
;; Smart Mode Line
;; *************************************************************
;; (use-package smart-mode-line
;;   :init
;;   (setq sml/theme 'respectful)
;;   :config
;;   (sml/setup))

;; *************************************************************
;; Diminish
;; *************************************************************
(use-package diminish)

;; *************************************************************
;; avy
;; *************************************************************
(use-package avy
  :config
  (setq avy-all-windows t)

  (defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
)

;; *************************************************************
;; pdf-tools
;; *************************************************************
;; (use-package pdf-tools)
;; (add-to-list 'load-path "~/build/pdf-continuous-scroll-mode.el/")
;; (require 'pdf-continuous-scroll-mode)

;; *************************************************************
;; flycheck
;; *************************************************************
(use-package flycheck)

;; *************************************************************
;; bison
;; *************************************************************
;; (use-package bison-mode)

;; *************************************************************
;; org-superstar
;; *************************************************************
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("●")
        org-hide-leading-stars t))

;; *************************************************************
;; lsp
;; *************************************************************
(use-package lsp-mode
  :config
  (setq lsp-headerline-breadcrumb-enable nil))
(use-package lsp-ui)

;; *************************************************************
;; ccls
;; *************************************************************
;; (use-package ccls)

;; *************************************************************
;; cpp
;; *************************************************************
(use-package cc-mode)

;; *************************************************************
;; Doom Mode Line
;; *************************************************************
;; (use-package doom-modeline
;;   :config
;;   (setq doom-modeline-support-imenu t)
;;   (setq doom-modeline-major-mode-icon nil)
;;   :init
;;   (doom-modeline-mode 1))

;; *************************************************************
;; Doom Themes
;; *************************************************************
;; (use-package doom-themes
;;   :config
;;   (load-theme 'doom-nord-light)
;;   (doom-themes-org-config))

;; *************************************************************
;; modus-themes
;; *************************************************************
(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t))

;; *************************************************************
;; Company
;; *************************************************************
(use-package company
  :diminish
  :config
  (global-company-mode 1))

(use-package eldoc
  :diminish)

;; ************************************************************
;; general
;; ************************************************************
(use-package general
  :after evil
  :config
  (general-evil-setup t)

  (general-define-key
   :states '(normal visual)
   "f" 'avy-goto-char-timer
   "/" 'isearch-forward
   "," 'consult-line
   "?" 'iedit-mode
   "C-w" 'evil-scroll-line-up)
  (general-define-key
   :states 'normal
   "P" 'consult-yank-from-kill-ring
   "n" 'isearch-repeat-forward
   "N" 'isearch-repeat-backward)

  (general-create-definer leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (leader-keys

   "SPC" '(execute-extended-command :which-key "M-x")
   "."   '(find-file :which-key "find files in current dir")
   ","   '(consult-buffer :which-key "switch buffer")

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
   "fs"  '(save-buffer :which-key "save-buffer")
   "fS"  '(write-file :which-key "write-file")

   "g"   '(:ignore t :which-key "git")
   "gg"   '(magit-status :which-key "magit")

   "h"   '(:ignore t :which-key "help")
   "ha"  '(apropos :which-key "apropos")
   "hf"  '(helpful-function :which-key "describe function")
   "hv"  '(helpful-variable :which-key "describe variable")
   "hk"  '(helpful-key :which-key "describe key")
   "hF"  '(describe-face :which-key "describe face")
   "h'"  '(describe-char :which-key "describe char")

   "q"   '(:ignore t :which-key "quit")
   "qK"  '(save-buffers-kill-emacs :which-key "save and quit")
   "qq"  '(save-buffers-kill-terminal :which-key "save and quit")

   "s"   '(:ignore t :which-key "search")
   "si"  '(consult-imenu :which-key "consult-imenu")
   "sd"  '(consult-ripgrep :which-key "consult-ripgrep")
   "sb"  '(consult-line :which-key "consult-line")
   "sf"  '(consult-locate :which-key "locate")

   "t"   '(rune/popup-eshell :which-key "eshell")

   "w"   '(:package evil
           :keymap evil-window-map
           :which-key "window")
   "ww" '(+hydra/window-nav/body :which-key "resize windows")))

;; *************************************************************
;; web-mode
;; *************************************************************
;; (defun add-company-to-web-mode ()
;;   (add-to-list 'company-backends 'company-web-html 'append))

;; (use-package web-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode) 'append)
;;   (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode) 'append)
;;   (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode) 'append)
;;   ;; (add-company-to-web-mode)
;;   )

;; (use-package emmet-mode
;;   :hook (css-mode web-mode html-mode php-mode)
;;   :config
;;   (setq emmet-move-cursor-between-quotes t))

;; *************************************************************
;; iedit
;; *************************************************************
(use-package iedit)

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


  (defhydra +hydra/window-nav (:hint nil)
    "
Resize: _h_: left  _j_: down  _k_: up  _l_: right "
    ("h" hydra-move-splitter-left       )
    ("j" hydra-move-splitter-down       )
    ("k" hydra-move-splitter-up         )
    ("l" hydra-move-splitter-right      )
    ("q" nil                           )))

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
         ("M-RET" . vertico-exit-input)
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous))
  :init
  (vertico-mode)
  (setq vertico-cycle t
        vertico-count 15))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; :load-path "extensions/"
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
;; Consult
;; ************************************************************
(use-package consult
  :bind (("C-x b" . consult-buffer))
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
  (("C-," . embark-act)
   ("C-x C-," . embark-act)
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
;; Which key
;; ************************************************************
(use-package which-key
  :diminish
  :init (which-key-mode))

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
;; Projectile
;; ************************************************************
(use-package projectile
  :diminish
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
;; (use-package dired
;;   :ensure nil
;;   :commands (dired dired-jump)
;;   :bind (("C-x C-j" . dired-jump))
;;   :init
;;   (add-hook 'dired-mode-hook #'dired-hide-details-mode)
;;   :config
;;   (evil-collection-define-key 'normal 'dired-mode-map
;;     "h" 'dired-single-up-directory
;;     "l" 'dired-single-buffer)))
  
  
;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

;; ************************************************************
;; Miscellaneous
;; ************************************************************
(use-package emacs
  :init
  ;; (add-hook 'emacs-startup-hook
  ;;           (lambda ()
  ;;             (setq gc-cons-threshold 16777216 ; 16mb
  ;;                   gc-cons-percentage 0.1)))
  ;; (defun doom-defer-garbage-collection-h ()
  ;;   (setq gc-cons-threshold most-positive-fixnum))
  ;; (defun doom-restore-garbage-collection-h ()
  ;;   (run-at-time
  ;;    1 nil (lambda () (setq gc-cons-threshold 16777216))))
  ;; (add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
  ;; (add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

  ;; (setq read-file-name-completion-ignore-case t
  ;;       read-buffer-completion-ignore-case t)

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
  (setq recentf-save-file (concat user-emacs-directory ".cache/recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 200)
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list)

  (setq custom-file (concat user-emacs-directory ".cache/custom.el"))

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

  (defun display-emacs-init-time ()
    "Displays the Emacs init time."
    (interactive)
    (message (emacs-init-time)))

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

  (setq blink-cursor-mode nil)

  (set-face-attribute 'variable-pitch nil
                      :family "Iosevka Custom Extended"
                      :slant 'oblique)

  ;; newline-without-break-of-line
  (defun newline-without-break-of-line ()
    "1. move to end of the line.
     2. insert newline with index"

    (interactive)
    (let ((oldpos (point)))
      (end-of-line)
      (newline-and-indent)))

  (global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

  )

(provide 'init)
;;; init.el ends here
