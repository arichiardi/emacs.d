;;; go-conf.el --- Go Config

;;; Commentary:

;;; Code:

(defun ar-emacs--configure-go ()
  "Go mode hook."
  (company-mode-on)
  (eldoc-mode t)
  (flycheck-mode 1)
  (ggtags-mode 1)
  (subword-mode 1))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . ar-emacs--configure-go))

;;; go-conf.el ends here
