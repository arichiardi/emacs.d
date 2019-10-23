(defun ar-emacs--racket-conf-hook ()
  "Racket mode hook."
  (enable-paredit-mode)
  (company-mode-on)
  (eldoc-mode t)
  (subword-mode t)
  (flyspell-prog-mode)
  (rainbow-delimiters-mode-enable))

(use-package racket-mode
  :mode "\\.rkt\\'"
  :bind (("C-c M-j" . racket-run)
         ("C-c C-c" . racket-send-last-sexp)
         ("C-c C-z" . racket-run-and-switch-to-repl))
  :init
  (add-hook 'racket-mode-hook #'ar-emacs--racket-conf-hook)
  (add-hook 'racket-repl-mode-hook #'ar-emacs--racket-conf-hook))
