;;; functions.el --- Description -*- lexical-binding: t; -*-
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
  (defun display-emacs-init-time ()
    "Displays the Emacs init time."
    (interactive)
    (message (emacs-init-time)))


  (defun delete-file-and-buffer ()
    "Kill the current buffer and delete the file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if filename (if (y-or-n-p (concat "Really delete '" filename "'?"))
                       (progn
                         (delete-file filename)
                         (message "Deleted %s." filename)
                         (kill-buffer)))
        (message "Failed."))))


  (defun newline-without-break-of-line ()
    "1. move to end of the line.
   2. insert newline with index"
    (interactive)
    (let ((oldpos (point)))
      (end-of-line)
      (newline-and-indent)))

  (defun projectile-emacs-directory ()
    "View the emacs user directory with projectile."
    (interactive)
    (projectile-find-file-in-directory user-emacs-directory))
  
  (defun jump-to-init-file ()
    "Jump to the emacs user directory with dired."
    (interactive)
    (find-file (concat user-emacs-directory "/init.el")))

  )

;; ************************************************************
;; ************************************************************
(provide 'functions)
;;; functions.el ends here
