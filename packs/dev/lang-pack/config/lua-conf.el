;;; lua-conf.el --- Lua Config

;;; Commentary:

;;; Code:

(defun ar-emacs--configure-lua ()
  (company-mode-on)
  (eldoc-mode t)
  (subword-mode 1))

(use-package lua-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . ar-emacs--configure-lua))

;;; go-conf.el ends here
