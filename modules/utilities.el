;;; utilities.el --- Description -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Code:

;; ************************************************************
;;; Avy
;; ************************************************************
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

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

;; ************************************************************
;;; Completion
;; ************************************************************
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)

  ;; (add-hook 'eshell-mode-hook
  ;;           (lambda ()
  ;;             (setq-local corfu-auto nil)
  ;;             (corfu-mode)))
  )

(use-package corfu-doc
  :bind (:map corfu-map
              ("M-p" . corfu-doc-scroll-down)
              ("M-n" . corfu-doc-scroll-up)
              ("M-d" . corfu-doc-toggle)
              )
  :hook
  (corfu-mode-hook . corfu-doc-mode)
  )

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package emacs
  :init (setq tab-always-indent 'complete))

(use-package dabbrev)

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  )

;; ************************************************************
;;; Flycheck
;; ************************************************************
(use-package flycheck)

;; ************************************************************
;;; Helpful
;; ************************************************************
(use-package helpful)

;; *************************************************************
;;; iedit
;; *************************************************************
(use-package iedit)

;; ************************************************************
;;; Magit
;; ************************************************************
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :config
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

;; ************************************************************
;;; Projectile
;; ************************************************************
(use-package projectile
  :diminish
  :init
  (setq projectile-cache-file
        (concat user-emacs-directory ".cache/projectile-cache"))
  (setq projectile-known-projects-file
        (concat user-emacs-directory ".cache/projectile-bookmarks.eld"))
  :config
  (projectile-mode))

;; ************************************************************
;; Smartparens
;; ************************************************************
;; (use-package smartparens
;;   :init
;;   (require 'smartparens-config)
;;   :config
;;   (setq smartparens-strict-mode t)
;;   (add-hook 'prog-mode-hook #'smartparens-mode))

;; ************************************************************
;; wgrep
;; ************************************************************
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t))

;; *************************************************************
;; *************************************************************
(provide 'utilities)
;;; utilities.el ends here
