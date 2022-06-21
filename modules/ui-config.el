;; -*- lexical-binding: t -*-
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
;; Diminish
;; *************************************************************
(use-package diminish)

;; *************************************************************
;; modus-themes
;; *************************************************************
(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t))

;; ************************************************************
;; Rainbow Delimiters
;; ************************************************************
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; *************************************************************
;; Smart Mode Line
;; *************************************************************
;; (use-package smart-mode-line
;;   :init
;;   (setq sml/theme 'respectful)
;;   :config
;;   (sml/setup))

;; ************************************************************
;; Which key
;; ************************************************************
(use-package which-key
  :diminish
  :init (which-key-mode))

;; ************************************************************
;; ************************************************************
(provide 'ui-config)
