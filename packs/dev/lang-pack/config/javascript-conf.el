;;; javascript-conf.el --- JavaScript Config

;;; Commentary:

;;; Code:

(defun ar-emacs--apply-if-not-editorconfig ()
  "JavaScript comint configuration hook."
  (unless (file-exists-p (expand-file-name (projectile-project-root) ".editorconfig"))
    (setq js2-basic-offset 2)))

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :bind (:map js2-mode-map
         ("C-c M-j" . js-comint-repl))
  :config
  (ar-emacs--apply-if-not-editorconfig)
  :hook
  ((js2-mode . company-mode)
   (js2-mode . smartparens-strict-mode)
   (js2-mode . editorconfig-mode)))

(defun ar-emacs--configure-js-comint ()
  "JavaScript comint configuration hook."
  (add-hook 'comint-output-filter-functions 'js-comint-process-output)
  (js-do-use-nvm))

(use-package js-comint
  :after js2-mode
  :functions js-comint-repl
  :hook (inferior-js-mode . ar-emacs--configure-js-comint)
  :bind (:map js2-mode-map
         ("C-c C-c" . js-comint-send-last-sexp)
         ("C-c C-v r" . js-comint-send-region)
         ("C-c C-k" . js-comint-send-buffer)
         ("C-c M-o" . js-comint-clear)
         ("C-c C-b" . js-comint-quit-or-cancel)
         ("C-c C-z" . js-comint-start-or-switch-to-repl)
         :map js-comint-mode-map
         ("C-c M-o" . js-clear)))

;;; javascript-conf.el ends here
