;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Code:

;; *************************************************************
;;; Initialize package sources
;; *************************************************************
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

(setq use-package-enable-imenu-support t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; ************************************************************
;;; Load Configuration
;; ************************************************************
(use-package emacs
  :init
  (add-to-list 'load-path (concat user-emacs-directory "modules/"))
  (require 'module-loader))

(provide 'init)
;;; init.el ends here
