(defun ar-emacs--define-racket-open-round-key ()
  "Assign ( to racket-smart-open-bracket.

  See https://github.com/greghendershott/racket-mode/issues/401."
  (let ((oldmap (cdr (assoc 'paredit-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "(") #'racket-smart-open-bracket)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(paredit-mode . ,newmap) minor-mode-overriding-map-alist)))

(defun ar-emacs--racket-conf-hook ()
  "Racket mode hook."
  (enable-paredit-mode)
  (ar-emacs--define-racket-open-round-key)
  (company-mode-on)
  (eldoc-mode t)
  (subword-mode t)
  (flyspell-prog-mode)
  (rainbow-delimiters-mode-enable))

(use-package racket-mode
  :mode "\\.rkt\\'"
  :bind (("C-c M-j" . racket-run)
         ("C-c SPC" . racket-align)
         :map racket-mode-map
         ("C-c C-c" . racket-send-last-sexp)
         :map racket-repl-mode-map
         ("C-w" . er/expand-region))
  :hook
  ((racket-mode . ar-emacs--racket-conf-hook)
   (racket-repl-mode . ar-emacs--racket-conf-hook))
  :custom
  (racket-smart-open-bracket-enable t "Enable smart open bracket"))
