;; -*- lexical-binding: t -*-
;; ************************************************************
;; Avy
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

;; *************************************************************
;; Company
;; *************************************************************
(use-package company
  :diminish
  :config
  (global-company-mode 1))

;; *************************************************************
;; Eldoc
;; *************************************************************
;; (use-package eldoc
;;   :diminish
;;   :config
;;   (setq global-eldoc-mode t))

;; ************************************************************
;; Flycheck
;; ************************************************************
(use-package flycheck)

;; ************************************************************
;; Helpful
;; ************************************************************
(use-package helpful)

;; *************************************************************
;; iedit
;; *************************************************************
(use-package iedit)

;; ************************************************************
;; Magit
;; ************************************************************
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

;; ************************************************************
;; Projectile
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
(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode))

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
