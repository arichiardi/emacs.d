;;; go-conf.el --- Go Config

;;; Commentary:

;;; Code:

(defun ar-emacs--configure-go ()
  "Racket mode hook."
  (company-mode-on)
  (eldoc-mode t)
  (flycheck-mode 1))

(use-package go-mode
  :mode ("\\.go\\'")
  :hook (go-mode . ar-emacs--configure-go))

;;; go-conf.el ends here
