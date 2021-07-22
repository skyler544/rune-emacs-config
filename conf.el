(setq default-frame-alist
      (append (list
               '(font . "Hack:style=Regular:size=16")
               '(height . 56)
               '(width . 81))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(load-theme 'tango-dark t)
(set-face-attribute 'default nil :height 200)

(add-to-list 'load-path "~/build/homegrown/emacs-which-key")
(require 'which-key)
(which-key-mode)
