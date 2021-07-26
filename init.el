;; *************************************************************
;; UI Configuration
;; *************************************************************
(setq default-frame-alist            ; font config
      (append (list
               '(font . "Hack:style=Regular:size=16")
               '(height . 50)
               '(width . 81))))

;; *************************************************************
;; Initialize package sources
;; *************************************************************
(require 'package)

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("org"    . "https://orgmode.org/elpa/")
        ("github" . "https://github.com/quelpa/quelpa")
        ("elpa"   . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; ************************************************************
;; Doom modeline
;; ************************************************************
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; ************************************************************
;; Doom themes
;; ************************************************************
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-nova t))

;; ************************************************************
;; Solaire Mode
;; ************************************************************
(use-package solaire-mode
  :ensure t
  :init (solaire-global-mode +1))

;; ************************************************************
;; Ivy
;; ************************************************************
(use-package ivy
  :ensure t
  :init (ivy-mode 1))

;; ************************************************************
;; Which key
;; ************************************************************
(use-package which-key
  :ensure t
  :init (which-key-mode))

;; ************************************************************
;; Keybindings
;; ************************************************************
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(defun scroll-up-a-line ()
  (interactive)
  (scroll-up-command 1))
(defun scroll-down-a-line ()
  (interactive)
  (scroll-down-command 1))
(global-set-key (kbd "C-j") 'scroll-up-a-line)
(global-set-key (kbd "C-k") 'scroll-down-a-line)

;; ************************************************************
;; Faces
;; ************************************************************
(set-face-attribute 'help-key-binding nil
                    :foreground "#c5d4dd"
                    :background nil
                    :box t                                    )

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
;; Custom
;; ************************************************************
(setq custom-file "~/build/homegrown/custom.el")
