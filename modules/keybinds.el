;;; keybinds.el --- Description -*- lexical-binding: t; -*-
;;
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Code:

;; *************************************************************
;;; Evil
;; *************************************************************
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-fine-undo t)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-move-beyond-eol t)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-redo)

  :config
  (evil-mode 1)
  (setq evil-shift-width 2)

  (evil-set-initial-state 'messages-buffer-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '(?` . ("`" . "'")) evil-surround-pairs-alist))))

(use-package evil-mc
  :after (evil general)
  :config
  (global-evil-mc-mode 1)
  (setq evil-mc-undo-cursors-on-keyboard-quit t)
  :bind (("C-j" . evil-mc-make-cursor-move-next-line)
         ("C-k" . evil-mc-make-cursor-move-prev-line)))

;; *************************************************************
;;; General
;; *************************************************************
(use-package general
  :after evil
  :config
  (general-evil-setup t)

  (general-define-key
   :states 'motion
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)

  (general-define-key
   :states '(normal visual)
   "f" 'avy-goto-char-timer
   "s" 'evil-surround-region
   "/" 'isearch-forward
   "," 'consult-line
   "?" 'iedit-mode
   "C-w" 'evil-scroll-line-up)

  (general-define-key
   :states 'normal
   "P" 'consult-yank-from-kill-ring
   "n" 'isearch-repeat-forward
   "N" 'isearch-repeat-backward)

  (general-create-definer leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (leader-keys

   "SPC" '(execute-extended-command :which-key "M-x")
   "."   '(find-file :which-key "find files in current dir")
   ","   '(consult-buffer :which-key "switch buffer")

   "b"   '(:ignore t :which-key "buffer")
   "bb"  '(consult-buffer :which-key "switch buffer")
   "bi"  '(ibuffer :which-key "ibuffer")
   "bk"  '(kill-current-buffer :which-key "kill current buffer")
   "bs"  '(save-buffer :which-key "save-buffer")
   "bS"  '(write-file :which-key "write-file")

   "cf"  '(lsp-format-buffer :which-key "format code")

   "f"   '(:ignore t :which-key "files")
   "a"   '(embark-act :which-key "embark-act")
   "fp"  '(projectile-find-file :which-key "find file in project")
   "fr"  '(consult-recent-file :which-key "recently opened files")
   "fs"  '(save-buffer :which-key "save-buffer")
   "fS"  '(write-file :which-key "write-file")

   "g"   '(:ignore t :which-key "git")
   "gg"   '(magit-status :which-key "magit")

   "h"   '(:ignore t :which-key "help")
   "ha"  '(apropos :which-key "apropos")
   "hf"  '(helpful-function :which-key "describe function")
   "hv"  '(helpful-variable :which-key "describe variable")
   "hk"  '(helpful-key :which-key "describe key")
   "hF"  '(describe-face :which-key "describe face")
   "h'"  '(describe-char :which-key "describe char")

   "pf" '(projectile-emacs-directory :which-key "jump to config directory")

   "q"   '(:ignore t :which-key "quit")
   "qK"  '(save-buffers-kill-emacs :which-key "save and quit")
   "qq"  '(save-buffers-kill-terminal :which-key "save and quit")

   "s"   '(:ignore t :which-key "search")
   "si"  '(consult-imenu :which-key "consult-imenu")
   "sd"  '(consult-ripgrep :which-key "consult-ripgrep")
   "sb"  '(consult-line :which-key "consult-line")
   "sf"  '(consult-locate :which-key "locate")

   "t"   '(eshell :which-key "eshell")

   "w"   '(:package evil
           :keymap evil-window-map
           :which-key "window")
   "ww" '(+hydra/window-nav/body :which-key "resize windows")))

;; *************************************************************
;;; Misc
;; *************************************************************

;; *************************************************************
;; *************************************************************
(provide 'keybinds)
;;; keybinds.el ends here
