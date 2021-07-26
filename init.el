;; *************************************************************
;; UI Configuration
;; *************************************************************
(column-number-mode)
(global-display-line-numbers-mode t)
(setq use-dialog-box nil)

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
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-nova t))

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

;; ************************************************************
;; Ivy Rich
;; ************************************************************
(use-package ivy-rich
  :init (ivy-rich-mode))

;; ************************************************************
;; Counsel
;; ************************************************************
(use-package counsel
  :bind ("M-x" . counsel-M-x))

;; ************************************************************
;; Swiper
;; ************************************************************
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
;; General
;; ************************************************************
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer rune/leader-keys)
    ;:keymaps '(normal insert visual emacs)
    ;:prefix "SPC"
    ;:global-prefix "C-SPC")

  (rune/leader-keys))

;; ************************************************************
;; Evil
;; ************************************************************
(use-package evil
:init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "/") 'swiper)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-scroll-line-up)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-scroll-line-down)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; ************************************************************
;; Keybindings
;; ************************************************************
(general-define-key "<escape>" 'keyboard-escape-quit)
(general-define-key "C-b" 'counsel-switch-buffer)

;; ************************************************************
;; Faces
;; ************************************************************
(set-face-attribute 'help-key-binding nil
                    :foreground "#c5d4dd"
                    :background nil
                    :box t                                    )

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
