;;; lisp-conf.el --- Lisp Config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun ar-emacs--configure-lisp ()
  "Racket mode hook."
  (enable-paredit-mode)
  (company-mode-on)
  (eldoc-mode t)
  (subword-mode t)
  (flyspell-prog-mode)
  (rainbow-delimiters-mode-enable))

(use-package emacs-lisp-mode
  :mode "\\.el\\'"
  :hook (emacs-lisp-mode . ar-emacs--configure-lisp)
  :bind (:map emacs-lisp-mode-map
         ("C-c C-c" . eir-eval-in-ielm)))

;;; lisp-conf.el ends here
