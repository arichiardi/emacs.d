;;; python-conf.el --- Python Config

;;; Commentary:

;;; Code:

(use-package python
  :config
  (use-package eval-in-repl-python)
  :bind (:map python-mode-map
         ("C-c C-c" . eir-eval-in-python)
         ("C-c C-k" . python-shell-send-buffer))
  :hook ((python-mode . company-mode)
         (python-mode . which-key-mode)
         (python-mode . subword-mode)
         (python-mode . smartparens-strict-mode)
         ;; Does not inherit from prog-mode
         (python-mode . hl-todo-mode)))

;;; python-conf.el ends here
