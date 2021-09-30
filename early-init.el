;; ************************************************************
;; Doom Performance Options
;; ************************************************************
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(setq load-prefer-newer noninteractive)

(setq ad-redefinition-action 'accept)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

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
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq default-frame-alist            ; font config
      (append (list
               '(font . "Hack:style=Regular:size=16")
               '(height . 50)
               '(width . 81))))

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-width-start t)
(setq use-dialog-box nil)
(add-hook 'prog-mode-hook (show-paren-mode))
(setq frame-resize-pixelwise t)
