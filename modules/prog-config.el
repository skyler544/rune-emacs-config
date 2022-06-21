;; -*- lexical-binding: t -*-
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
(use-package ccls)

;; *************************************************************
;; cpp
;; *************************************************************
(use-package cc-mode)

;; *************************************************************
;; Format All
;; *************************************************************
(use-package format-all)

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
;; *************************************************************
(provide 'prog-config)
