;; ************************************************************
;; Doom Performance Options
;; ************************************************************
;; (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
;;       gc-cons-percentage 0.6)

(setq load-prefer-newer noninteractive)

(setq ad-redefinition-action 'accept)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq idle-update-delay 1.0)

(setq redisplay-skip-fontification-on-input t)

(setq frame-inhibit-implied-resize t)
(setq initial-major-mode 'fundamental-mode)

;; ************************************************************
;; UI Setup
;; ************************************************************
(menu-bar-mode -1)
(push '(menu-bar-lines . 0) default-frame-alist)
(tool-bar-mode -1)
(push '(tool-bar-lines . 0) default-frame-alist)
(scroll-bar-mode -1)

(setq default-frame-alist 
      (append (list
               ;; '(font . "Liberation Mono")
               ;; '(font . "FiraCode Nerd Font Mono")
               ;; '(font . "Hack")
               ;; '(font . "Roboto Mono")
               ;; '(font . "IBM Plex Mono")
               '(font . "Iosevka Custom Extended:size=16")
               '(height . 50)
               '(width . 81))))

(column-number-mode)
(setq display-line-numbers-width-start t)
(setq use-dialog-box nil)
(add-hook 'prog-mode-hook (show-paren-mode))
(setq frame-resize-pixelwise t)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq custom-safe-themes t)
;; (setq custom-theme-directory "~/build/rune/themes/")

;; *************************************************************
;; Theme
;; *************************************************************
(load-theme 'modus-operandi)
;; (load-theme 'mindre)
