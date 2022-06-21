;; -*- lexical-binding: t -*-
;; *************************************************************
;; Emacs
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
  )

;; *************************************************************
;; *************************************************************
(provide 'module-loader)
