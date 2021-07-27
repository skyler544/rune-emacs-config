;; *************************************************************
;; UI Configuration
;; *************************************************************
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-width-start t)
(setq use-dialog-box nil)
(add-hook 'prog-mode-hook (show-paren-mode))
(setq frame-resize-pixelwise t)

;; *************************************************************
;; Initialize package sources
;; *************************************************************
(require 'package)

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("org"    . "https://orgmode.org/elpa/")
        ("elpa"   . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; ************************************************************
;; Doom modeline
;; ************************************************************
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; ************************************************************
;; Doom themes
;; ************************************************************
(use-package doom-themes
  :after doom-modeline
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (setq custom-theme-directory "~/build/rune/themes/")
  (load-theme 'doom-sourcerer-alt t))


;; ************************************************************
;; Ivy
;; ************************************************************
(use-package ivy
  :bind (:map ivy-minibuffer-map
          ("C-j" . ivy-next-line)
          ("C-k" . ivy-previous-line)
          ("C-r" . counsel-minibuffer-history))
  :init (ivy-mode 1)
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :init (ivy-rich-mode))

(use-package counsel
  :bind ("M-x" . counsel-M-x)
  :config
  (setq counsel-fzf-cmd "fd . '/home/clock/.config/' '/home/clock/'\
                            '/home/clock/.local/bin'\
                            | fzf -f \"%s\""))

(use-package swiper)

;; ************************************************************
;; Which key
;; ************************************************************
(use-package which-key
  :init (which-key-mode))

;; ************************************************************
;; Rainbow Delimiters
;; ************************************************************
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ************************************************************
;; Helpful
;; ************************************************************
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; ************************************************************
;; Evil
;; ************************************************************
(use-package evil
:init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-fine-undo t)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-move-beyond-eol t)
  :config
  (evil-mode 1)
  (setq evil-shift-width 2)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "/") 'swiper)
  (define-key evil-normal-state-map (kbd "C-w") 'evil-scroll-line-up)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-mc
  :after (evil general)
  :config
  (global-evil-mc-mode 1)
  (setq evil-mc-undo-cursors-on-keyboard-quit t)
  :bind (("C-j" . evil-mc-make-cursor-move-next-line)
         ("C-k" . evil-mc-make-cursor-move-prev-line)))

;; ************************************************************
;; Org
;; ************************************************************
(defun rune/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 0)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . rune/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'org-indent)

;; ************************************************************
;; General
;; ************************************************************
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
   "SPC" '(counsel-M-x :which-key "M-x")
   "."   '(counsel-find-file :which-key "find files in current dir")
   "f"   '(:ignore t :which-key "files")
   "ff"  '(counsel-fzf :which-key "fzf")
   "fp"  '(projectile-find-file :which-key "find file in project")
   "fo"  '(rune/browse-org :which-key "browse org folder")
   "fi"  '(rune/edit-init :which-key "edit init file")
   "fI"  '(rune/browse-init :which-key "browse init dir")
   "fw"  '(rune/browse-work :which-key "browse work dir")
   "fm"  '(rune/browse-mega :which-key "browse mega")
   "fs"  '(save-buffer :which-key "save-buffer")
   "fS"  '(write-file :which-key "write-file")
   "g"   '(magit :which-key "git")
   "q"   '(:ignore t :which-key "quit")
   "qK"  '(save-buffers-kill-emacs :which-key "save and quit")
   "b"   '(:ignore t :which-key "buffer")
   "bb"  '(counsel-switch-buffer :which-key "switch buffer")
   "bi"  '(ibuffer :which-key "ibuffer")
   "bk"  '(kill-current-buffer :which-key "kill current buffer")
   "bs"  '(save-buffer :which-key "save-buffer")
   "bS"  '(write-file :which-key "write-file")
   "h"   '(:ignore t :which-key "help")
   "ha"  '(apropos :which-key "apropos")
   "ht"  '(counsel-load-theme :which-key "load theme")
   "hf"  '(counsel-describe-function :which-key "describe function")
   "hv"  '(counsel-describe-variable :which-key "describe variable")
   "hk"  '(helpful-key :which-key "describe key")
   "hF"  '(counsel-faces :which-key "describe face")
   "o"   '(:package org
	   :keymap org-mode-map
	   :which-key "org")
   "w"   '(:package evil
	   :keymap evil-window-map
	   :which-key "window")
   "p"   '(:package projectile
           :keymap projectile-command-map
           :which-key "project")))

;; ************************************************************
;; Projectile
;; ************************************************************
(use-package projectile
  :config (projectile-mode))
(use-package counsel-projectile
 :after projectile
 :config
 (counsel-projectile-mode 1))

;; ************************************************************
;; Magit
;; ************************************************************
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; ************************************************************
;; Miscellaneous Functions
;; ************************************************************
(defun rune/edit-init ()
  "Edit the init.el file."
  (interactive)
  (find-file "~/build/rune/init.el"))

(defun rune/browse-init ()
  "Browse the init folder."
  (interactive)
  (counsel-find-file "~/build/rune/"))

(defun rune/browse-mega ()
  "Browse Mega."
  (interactive)
  (counsel-find-file "~/mega/"))

(defun rune/browse-work ()
  "Browse the work folder."
  (interactive)
  (counsel-find-file "~/mega/work/"))

(defun rune/browse-org ()
  "Browse the org folder."
  (interactive)
  (counsel-find-file "~/mega/org/"))

;; ************************************************************
;; Keybindings
;; ************************************************************
(general-define-key "<escape>" 'keyboard-escape-quit)
(general-define-key "C-b" 'counsel-switch-buffer)

;; ************************************************************
;; Faces
;; ************************************************************
(set-face-attribute 'doom-modeline-buffer-path nil
                    :foreground "#87875f"                     )
(set-face-attribute 'ivy-cursor nil
		    :foreground "#c5d4dd"
                    :background "#3e545c"                     )
(set-face-attribute 'xref-match nil
		    :foreground "#c5d4dd"
                    :background "#3e545c"                     )
(set-face-attribute 'match nil
		    :foreground "#c5d4dd"
                    :background "#3e545c"                     )
(set-face-attribute 'swiper-line-face nil
		    :foreground "#c5d4dd"
                    :background "#708591"                     )
(set-face-attribute 'swiper-match-face-4 nil
		    :foreground "#c5d4dd"
                    :background "#3e545c"                     )
(set-face-attribute 'swiper-match-face-3 nil
		    :foreground "#c5d4dd"
                    :background "#3e545c"                     )
(set-face-attribute 'swiper-match-face-2 nil
		    :foreground "#c5d4dd"
                    :background "#3e545c"                     )
(set-face-attribute 'swiper-match-face-1 nil
		    :foreground "#c5d4dd"
                    :background "#3e545c"                     )
(set-face-attribute 'swiper-background-match-face-4 nil
		    :foreground "#c5d4dd"
                    :background "#3e545c"                     )
(set-face-attribute 'swiper-background-match-face-3 nil
		    :foreground "#c5d4dd"
                    :background "#3e545c"                     )
(set-face-attribute 'swiper-background-match-face-2 nil
		    :foreground "#c5d4dd"
                    :background "#3e545c"                     )
(set-face-attribute 'swiper-background-match-face-1 nil
		    :foreground "#c5d4dd"
                    :background "#3e545c"                     )
(set-face-attribute 'evil-ex-lazy-highlight nil
		    :foreground "#c5d4dd"
                    :background "#3e545c"                     )
(set-face-attribute 'ivy-minibuffer-match-face-2 nil
		    :foreground "#74969F"
                    :background nil                           )
(set-face-attribute 'ivy-minibuffer-match-face-3 nil
		    :foreground "#D39735"
                    :background nil                           )
(set-face-attribute 'doom-modeline-evil-normal-state nil
                    :weight 'normal                           )
(set-face-attribute 'doom-modeline-evil-insert-state nil
                    :weight 'normal                           )
(set-face-attribute 'doom-modeline-evil-visual-state nil
                    :weight 'normal                           )
(set-face-attribute 'doom-modeline-evil-emacs-state nil
                    :weight 'normal                           )

;; ************************************************************
;; Garbage collection
;; ************************************************************
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))
(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))
(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

;; ************************************************************
;; Miscellaneous
;; ************************************************************
(setq make-backup-files nil) ; this stops the annoying ~ files

;; ************************************************************
;; Custom
;; ************************************************************
(setq custom-file "~/build/rune/custom.el")
