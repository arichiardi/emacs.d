;;; init.el --- Language Pack

;;; Commentary:

;;; Code:

(use-package volatile-highlights
  :diminish
  :config
  (volatile-highlights-mode t))

(use-package gfm-mode
  :mode "README\\.md\\'")

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
(live-load-config-file "javascript-conf.el")
(live-load-config-file "go-conf.el")

;;; init.el ends here
