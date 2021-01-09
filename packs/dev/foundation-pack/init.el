(use-package dash
  :config (dash-enable-font-lock))

(use-package sh-mode
  :mode ("\\.bats\\'" "\\.bashrc")
  :hook ((sh-mode . company-mode)
         (sh-mode . rainbow-delimiters-mode)))

(require 'dircolors)
(require 'smooth-scrolling)
(require 'buffer-move)

(live-add-pack-lib "s")
(require 's)
(live-add-pack-lib "epl")
(require 'epl)
(live-add-pack-lib "pkg-info")
(require 'pkg-info)

(live-load-config-file "backup-dir-conf.el")
(live-load-config-file "util-fns.el")
(live-load-config-file "built-in.el")
(live-load-config-file "cosmetic.el")
(live-load-config-file "smex-conf.el")
(live-load-config-file "tramp-conf.el")
(live-load-config-file "mouse-conf.el")
(live-load-config-file "key-chord-conf.el")
(live-load-config-file "recentf-conf.el")
(live-load-config-file "popwin-conf.el")
(live-load-config-file "shell-conf.el")
(live-load-config-file "spelling-conf.el")
(live-load-config-file "win-switch-conf.el")
(live-load-config-file "zone-conf.el")
(live-load-config-file "monkey-patch.el")
(live-load-config-file "completion-conf.el")
(live-load-config-file "helm-conf.el")
(live-load-config-file "projectile-conf.el")
(live-load-config-file "ediff-conf.el")
(live-load-config-file "lsp-conf.el")
(live-load-config-file "paredit-conf.el")
(live-load-config-file "live.el")

(when (eq system-type 'darwin)
  (live-load-config-file "osx.el"))

(use-package transient
  :config
  (setq transient-history-file (expand-file-name (expand-file-name "history.el" "transient")
                                                 live-tmp-dir)))

(use-package ws-butler
  :diminish)

(ws-butler-global-mode)

(use-package editorconfig
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode "Only trim touched lines"))
