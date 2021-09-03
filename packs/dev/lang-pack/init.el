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

(defun ar-emacs--point-after-equal-p (id action context)
  "Return t if point is after \\=, nil otherwise.  This predicate
is only tested on \"insert\" action."
  (when (eq action 'insert)
    (save-excursion
      (= (preceding-char) ?=))))

(use-package web-mode
  :mode ("\\.jsx\\'" "\\.tsx\\'")
  :hook
  ((web-mode . company-mode)
   ;; Cannot write inline functions otherwise
   ;; See: https://github.com/Fuco1/smartparens/issues/1101
   ;; (web-mode . smartparens-strict-mode)
   (web-mode . smartparens-mode)
   (web-mode . subword-mode)
   (web-mode . eldoc-mode)
   (web-mode . which-key-mode))
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")))
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-current-element-highlight t)
  (company-transformers '(company-sort-by-backend-importance))
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (sp-local-pair 'web-mode "<" nil :unless '(ar-emacs--point-after-equal-p)))

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
