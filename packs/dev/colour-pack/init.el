(global-hl-line-mode 1)

(live-load-config-file "live-fontify-hex-conf.el")

(set-face-attribute 'default nil :height 120)

(use-package modus-vivendi-theme
  :config (load-theme 'modus-vivendi t))
