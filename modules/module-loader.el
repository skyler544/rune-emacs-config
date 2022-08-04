;;; module-loader.el --- Description -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Code:

;; *************************************************************
;;; Emacs
;; *************************************************************
(use-package emacs
  :init
  (require 'utilities)
  (require 'misc-config)
  (require 'functions)
  (require 'movec)

  (require 'prog-config)
  (require 'org-config)

  (require 'keybinds)

  (require 'ui-config)
  (require 'hydra-config)

  (require 'misc-packages)
  )

;; *************************************************************
;; *************************************************************
(provide 'module-loader)
