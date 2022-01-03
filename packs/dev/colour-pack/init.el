;;; init.el --- Colour Pack

;;; Commentary:

;;; Code:

(global-hl-line-mode 1)

(live-load-config-file "live-fontify-hex-conf.el")

(use-package modus-vivendi-theme
  :config (load-theme 'modus-vivendi t))

;;; init.el ends here
