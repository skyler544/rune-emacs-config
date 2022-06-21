;; -*- lexical-binding: t -*-
;; *************************************************************
;; Initialize package sources
;; *************************************************************
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(setq use-package-enable-imenu-support t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; ************************************************************
;; Miscellaneous
;; ************************************************************
(use-package emacs
  :init
  (add-to-list 'load-path (concat user-emacs-directory "modules/"))
  (require 'module-loader))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dired which-key wgrep vertico use-package smartparens smart-mode-line rainbow-delimiters projectile org-bullets orderless modus-themes marginalia magit lsp-ui iedit hydra helpful general format-all flycheck evil-surround evil-mc evil-collection embark-consult diminish consult-dir company avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
