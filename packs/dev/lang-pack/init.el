;;; init.el --- Language Pack

;;; Commentary:

;;; Code:

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package gfm-mode
  :mode "README\\.md\\'")

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom
  (markdown-open-command "function md() { pandoc \"$1\" | lynx -stdin; }; md" "Render markdown using pandoc."))

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

(live-load-config-file "flycheck-conf.el")
(live-load-config-file "yaml-conf.el")
(live-load-config-file "plantuml-conf.el")
(live-load-config-file "elisp-conf.el")
(live-load-config-file "lisp-conf.el")
(live-load-config-file "python-conf.el")
(live-load-config-file "ocaml-conf.el")
(live-load-config-file "lua-conf.el")
(live-load-config-file "java-conf.el")
