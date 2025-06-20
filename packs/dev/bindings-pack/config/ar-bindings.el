;;; ar-bindings.el --- AR Emacs bindings -*- lexical-binding: t; -*-

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
(global-set-key (kbd "C-c C-o") 'delete-blank-lines)

(bind-keys :prefix-map ar-emacs-llm-prefix-map
           :prefix-docstring "Prefix key for all things LLM."
           :prefix "C-c x"
           ("<return>" . gptel)
           ("q" . gptel-abort)
           ("h" . mcp-hub)
           ("f" . gptel-add)
           ("r" . gptel-rewrite)
           ("s" . gptel-send)
           ("t" . gptel-tools)
           ("m"   . gptel-menu))

(bind-keys :prefix-map ar-emacs-clojure-prefix-map
           :prefix-docstring "Prefix key for Clojure."
           :prefix "C-c c"
           ("b" . ar-emacs-clj-eval-all-let-bindings)
           ("d" . cider-debug-defun-at-point)
           ("s" . clojure-sort-ns))

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
(global-set-key (kbd "M-g c") 'avy-goto-char)
(global-set-key (kbd "M-g g") 'avy-goto-line)

;; Projectiln
(global-set-key (kbd "s-p") 'projectile-persp-switch-project)
(global-set-key (kbd "s-.") 'projectile-find-tag)
(global-set-key (kbd "s-M-t") 'projectile-toggle-between-implementation-and-test)

;; Consult
(global-set-key (kbd "s-s") 'ar-emacs-consult-ripgrep)
(global-set-key (kbd "s-g") 'ar-emacs-consult-git-grep)
(global-set-key (kbd "s-f") 'projectile-find-file-dwim)

(provide 'ar-bindings)

;;; ar-bindings.el ends here
