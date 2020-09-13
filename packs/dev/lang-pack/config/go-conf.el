;;; go-conf.el --- Go Config

;;; Commentary:

;;; Code:

(use-package go-mode
  :mode "\\.go\\'"

  :hook
  (go-mode . smartparens-strict-mode)
  (go-mode . editorconfig-mode)
  (go-mode . subword-mode)
  (go-mode . flycheck-mode)
  (go-mode . company-mode-on)

  :config
  (go-eldoc-setup)

  :bind (:map go-mode-map
         ("C-c C-f g" . godoc-at-point))
  :custom
  (godoc-at-point-function 'godoc-gogetdoc "Use gogetdoc for documentation at point"))

(use-package go-complete
  :after go-mode
  :config
  (add-hook 'completion-at-point-functions 'go-complete-at-point))

(use-package go-eldoc
  :after go-mode)

;;; go-conf.el ends here
