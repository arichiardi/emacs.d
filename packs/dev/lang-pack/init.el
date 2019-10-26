;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Pack

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-open-command "function md() { pandoc \"$1\" | lynx -stdin; }; md"))

(use-package lsp-java
  :after lsp
  :config
  (add-hook 'java-mode-hook #'company-mode)
  (add-hook 'java-mode-hook #'flycheck-mode))

(live-load-config-file "flycheck-conf.el")
(live-load-config-file "yaml-conf.el")
(live-load-config-file "auto-modes.el")
(live-load-config-file "js2-conf.el")
(live-load-config-file "plantuml-conf.el")
(live-load-config-file "elisp-conf.el")
