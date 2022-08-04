;;; org-config.el --- Description -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Code:

;; *************************************************************
;;; org-superstar
;; *************************************************************
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("●")
        org-hide-leading-stars t))

;; *************************************************************
;; *************************************************************
(provide 'org-config)
;;; org-config.el ends here
