;;; smartparens-conf.el --- Smartparens Config

;;; Commentary:
;;
;; Also check
;; https://ebzzry.github.io/emacs-pairs.html
;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
;;
;;; Code:

(use-package smartparens-mode
  :hook (shell-mode)
  :config
  ;; load default config
  (require 'smartparens-config)
  (require 'dash)
  (defun ar-emacs--sp-add-space-after-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (forward-char (sp-get-pair id :cl-l))
        (when (or (eq (char-syntax (following-char)) ?w)
                  (looking-at (sp--get-opening-regexp)))
          (insert " ")))))

  (defun ar-emacs--sp-add-space-before-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (backward-char (length id))
        (when (or (eq (char-syntax (preceding-char)) ?w)
                  (and (looking-back (sp--get-closing-regexp))
                       (not (eq (char-syntax (preceding-char)) ?'))))
          (insert " ")))))

  (defvar sp-clojure-prefix "\\(?:[@`'#~,_?^]+\\)"
    "Prefix used in `sp-sexp-prefix' for clojure modes.")

  (sp-with-modes
      '(java-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))

  (--each '(java-mode)
    (add-to-list 'sp-sexp-suffix (list it 'regexp "")))
  )

;;; smartparens-conf.el ends here
