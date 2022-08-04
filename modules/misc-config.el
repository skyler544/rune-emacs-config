;;; misc-config.el --- Description -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; Code:

;; ************************************************************
;;; Emacs
;; ************************************************************
(use-package emacs
  :init
  (setq byte-compile-warnings nil)
  (setq warning-suppress-log-types '((comp)))
  (setq warning-suppress-types '((comp)))
  (setq enable-recursive-minibuffers t)
  (setq make-backup-files nil)
  ;; (setq truncate-lines t)
  (setq word-wrap nil)
  (setq-default indent-tabs-mode nil)
  (setq echo-keystrokes 0.02)
  (setq completion-ignore-case t)
  (setq blink-cursor-mode nil)

  (require 'recentf)
  (setq recentf-save-file (concat user-emacs-directory ".cache/recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 200)
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list)

  (setq custom-file (concat user-emacs-directory ".cache/custom.el"))
  (setq auth-sources (concat user-emacs-directory ".authinfo"))

  (setq tramp-auto-save-directory (concat user-emacs-directory ".cache/tramp-autosave"))
  (setq backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not
                (let ((method (file-remote-p name 'method)))
                  (when (stringp method)
                    (member method '("su" "sudo"))))))))

  (setq eshell-history-file-name (concat user-emacs-directory ".cache/eshell-history"))
  (setq savehist-file (concat user-emacs-directory ".cache/minibuffer-history"))
  (setq transient-history-file (concat user-emacs-directory ".cache/transient-history")))

;; ************************************************************
;;; savehist
;; ************************************************************
(use-package savehist
  :init
  (savehist-mode))

;; ************************************************************
;; ************************************************************
(provide 'misc-config)
;;; misc-config.el ends here
