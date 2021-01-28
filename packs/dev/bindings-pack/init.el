(live-load-config-file "default-bindings.el")

(use-package which-key
  :init
  (which-key-setup-side-window-right-bottom)

  :custom
  (which-key-idle-delay 0.25))
