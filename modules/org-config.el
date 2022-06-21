;; -*- lexical-binding: t -*-
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
;; *************************************************************
(provide 'org-config)
