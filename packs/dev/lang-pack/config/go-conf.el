;;; go-conf.el --- Go Config

;;; Commentary:

;;; Code:

(use-package go-eldoc)

(defun ar-emacs--configure-go ()
  "Go mode hook."
  (company-mode 1)
  (flycheck-mode 1)
  (ggtags-mode 1)
  (subword-mode 1)
  (go-eldoc-setup)
  (smartparens-strict-mode))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . ar-emacs--configure-go)
  :bind (:map go-mode-map
         ("C-c C-f g" . godoc-at-point))
  :custom
  (godoc-at-point-function 'godoc-gogetdoc "Use gogetdoc for documentation at point"))

(use-package go-complete
  :after go-mode
  :config
  (add-hook 'completion-at-point-functions 'go-complete-at-point))
;;; go-conf.el ends here
