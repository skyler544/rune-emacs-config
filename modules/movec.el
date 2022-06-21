;; -*- lexical-binding: t -*-
;; ************************************************************
;; Vertico
;; ************************************************************
(use-package vertico
  :config
  (setq file-name-shadow-properties '(invisible t intangible t))
  (file-name-shadow-mode +1)
  :bind (:map vertico-map
         ("M-RET" . vertico-exit-input)
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous))
  :init
  (vertico-mode)
  (setq vertico-cycle t
        vertico-count 15))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; :load-path "extensions/"
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)))

(use-package savehist
  :init
  (savehist-mode))

;; ************************************************************
;; Orderless
;; ************************************************************
(use-package orderless
  :custom
  (completion-styles '(substring orderless)))

;; ************************************************************
;; Marginalia
;; ************************************************************
(use-package marginalia
  :bind (:map vertico-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; ************************************************************
;; Consult
;; ************************************************************
(use-package consult
  :bind (("C-x b" . consult-buffer))
  :config
  (consult-customize consult-buffer :preview-key (kbd "M-/"))

  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)
  (advice-add #'multi-occur :override #'consult-multi-occur)

  (setq consult-narrow-key "<")

  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-d" . consult-dir)
         ("C-f" . consult-dir-jump-file)))

;; ************************************************************
;; Embark
;; ************************************************************
(use-package embark
  :bind
  (("C-," . embark-act)
   ("C-x C-," . embark-act)
   ("C-h B" . embark-bindings))

  :init
  (general-define-key
   :keymaps 'minibuffer-local-map
   "C-;" 'embark-act)

  :config
  (setq embark-prompter #'embark-completing-read-prompter)
  (setq embark-indicators
        '(embark-minimal-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))

(defun +embark-sudo-edit ()
  (interactive)
  (find-file (concat "/sudo:root@localhost:"
                     (expand-file-name (read-file-name "Find file as root: ")))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ************************************************************
;; ************************************************************
(provide 'movec)
