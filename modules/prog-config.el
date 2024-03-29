;;; prog-config.el --- Description -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Code:

;; *************************************************************
;;; lsp
;; *************************************************************
(use-package lsp-mode
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-phpactor-path "/usr/local/bin/phpactor")
  :hook (c++-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package lsp-ui)

;; *************************************************************
;;; ccls
;; *************************************************************
(use-package ccls)

;; *************************************************************
;;; cpp
;; *************************************************************
(use-package cc-mode)

;; *************************************************************
;;; Format All
;; *************************************************************
(use-package format-all)

;; *************************************************************
;;; php
;; *************************************************************
(use-package php-mode
  :config

  (defun +php-interpret-file ()
    "Interpret the current file."
    (interactive)
    (shell-command (concat "php " (buffer-file-name)) "*PHP*" "*PHP-ERR*"))

  (general-define-key
   :states 'normal
   "SPC php" '+php-interpret-file))

;; *************************************************************
;;; web-mode
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
;;; Builtin
;; *************************************************************
(use-package elec-pair :hook (prog-mode . electric-pair-mode))

;; *************************************************************
;; *************************************************************
(provide 'prog-config)
;;; prog-config.el ends here
