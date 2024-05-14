;;; smartparens-bindings.el --- Smartparens bindings

;;; Commentary:
;; Rebind Smartparens
;; Also check
;; https://ebzzry.github.io/emacs-pairs.html
;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el

;;; Code:
(with-eval-after-load "smartparens"
  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-<rigth>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-<left>") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-r") 'sp-raise-sexp)
  (define-key smartparens-mode-map (kbd "M-S") 'sp-split-sexp)
  (define-key smartparens-mode-map (kbd "M-s") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "M-j") 'sp-join-sexp)
  (define-key smartparens-mode-map (kbd "M-T") 'sp-transpose-sexp)
  (define-key smartparens-mode-map (kbd "M-d") 'sp-kill-sexp))

;;; smartparens-bindings.el ends here
