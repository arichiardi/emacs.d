;;; ar-bindings.el --- AR Emacs bindings

;;; Commentary:
;;
;; Custom bindings for AR, see default-bindings for groupings

;;; Code:

(global-set-key (kbd "RET") 'ar-emacs-return)
(global-set-key (kbd "M-[ M-DEL") 'ar-emacs-paredit-kill-parent-sexp)

(global-set-key (kbd "M-/") 'ar-emacs-narrow-or-widen-dwim)
(global-set-key (kbd "C-/") 'ar-emacs-comment-or-uncomment-region-or-line)

(global-set-key (kbd "C-v") 'cua-paste)
(global-set-key (kbd "C-c C-o") 'delete-blank-lines)
(global-set-key (kbd "C-M-\\") 'live-delete-whitespace-except-one)

(bind-keys :prefix-map ar-emacs-llm-prefix-map
           :prefix-docstring "Prefix key for all things LLM."
           :prefix "C-c C-x"
           ("<tab>" .  minuet-show-suggestion)
           ("M-<tab>" . minuet-complete-with-minibuffer)
           ("b" . gptel)
           ("C-b" . gptel)
           ("s"   . gptel-send)
           ("C-s" . gptel-send)
           ("<return>"   . gptel-menu)
           ("C-<return>" . gptel-menu)
           ("q" . gptel-abort)
           ("C-q" . gptel-abort)
           ("r"   . gptel-rewrite)
           ("C-r" . gptel-rewrite))

(bind-keys :prefix-map ar-emacs-text-prefix-map
           :prefix-docstring "Prefix key for text manipulation."
           :prefix "C-c t"
           ("a" . avy-resume)
           ("d" . duplicate-dwim)
           ("s" . ar-emacs-sprunge)
           ("u" . untabify-buffer)
           ("w c" . whitespace-cleanup)
           ("l p" . ar-emacs-move-line-up)
           ("l n" . ar-emacs-move-line-down))

(bind-keys :prefix-map ar-emacs-window-prefix-map
           :prefix-docstring "Prefix key for window manipulation."
           :prefix "C-c w"
           ("s" . ar-emacs-resize-window)
           ("t" . ar-emacs-toggle-window-split))

;; Some Intellj bindings I am used to
(global-set-key (kbd "C-y") 'kill-whole-line)
(global-set-key (kbd "C-w") 'er/expand-region)

(global-set-key "\033[32;16~" 'set-rectangular-region-anchor)
(global-set-key (kbd "s-SPC") 'set-rectangular-region-anchor)

;; Avy
(global-set-key (kbd "C-:") 'avy-goto-char-timer)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g g") 'avy-goto-line)

;;; ar-bindings.el ends here
