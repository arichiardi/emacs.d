;;; ar-bindings.el --- AR Emacs bindings

;;; Commentary:
;;
;; Custom bindings for AR, see default-bindings for groupings

;;; Code:

(global-set-key (kbd "RET") 'ar-emacs-return)
(global-set-key (kbd "M-[ M-DEL") 'ar-emacs-paredit-kill-parent-sexp)

(global-set-key (kbd "M-/") 'ar-emacs-narrow-or-widen-dwim)
(global-set-key (kbd "C-/") 'ar-emacs-comment-or-uncomment-region-or-line)

(global-set-key (kbd "C-c w t") 'ar-emacs-toggle-window-split)
(global-set-key (kbd "C-c w s") 'ar-emacs-resize-window)

(global-set-key (kbd "C-c t s") 'ar-emacs-sprunge)

(with-eval-after-load "prog-mode"
  (define-key prog-mode-map (kbd "M-p") 'ar-emacs-move-line-up)
  (define-key prog-mode-map (kbd "M-n") 'ar-emacs-move-line-down))

;;; ar-bindings.el ends here
