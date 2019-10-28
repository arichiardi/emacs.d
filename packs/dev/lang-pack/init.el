;;; init.el --- Language Pack

;;; Commentary:

;;; Code:

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-open-command "function md() { pandoc \"$1\" | lynx -stdin; }; md"))

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :custom
  (js2-basic-offset 2 "Set offset to 2"))

(use-package json-mode
  :mode "\\.json\\'"
  :custom
  ((json-reformat:indent-width 2 "Set width to 2")
   (js-indent-level 2 "Set indent level to 2")))

(use-package lsp-java
  :after lsp
  :config
  (add-hook 'java-mode-hook #'company-mode)
  (add-hook 'java-mode-hook #'flycheck-mode))

(live-load-config-file "flycheck-conf.el")
(live-load-config-file "yaml-conf.el")
(live-load-config-file "plantuml-conf.el")
(live-load-config-file "elisp-conf.el")
