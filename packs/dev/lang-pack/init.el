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

(use-package web-mode
  :mode ("\\.jsx\\'" "\\.tsx\\'")
  :hook
  ((web-mode . company-mode)
   (web-mode . smartparens-strict-mode)
   (web-mode . subword-mode)
   (web-mode . eldoc-mode)
   (web-mode . which-key-mode)
   (web-mode . tide-setup)
   (web-mode . tide-hl-identifier-mode)
   (web-mode . ar-emacs--setup-tsx-company-backends))
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")))
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-current-element-highlight t)
  (company-transformers '(company-sort-by-backend-importance))
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append))

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
(live-load-config-file "typescript-conf.el")

;;; init.el ends here
