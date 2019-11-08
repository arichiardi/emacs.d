(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-enable-snippet nil))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (add-hook 'lsp-mode-hook #'lsp-ui-mode))

(use-package company-lsp
  :defer t
  :after company)
