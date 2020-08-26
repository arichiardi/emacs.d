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
         (python-mode . smartparens-strict-mode)))

;;; python-conf.el ends here
