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

(defun ar-emacs--point-after-equal-p (id action _context)
  "Return t if point is after \\=, nil otherwise.  This predicate
is only tested on \"insert\" action.

See: https://github.com/Fuco1/smartparens/issues/1101"
  (when (eq action 'navigate)
    (save-excursion
      (backward-char 1)
      (equal (preceding-char) ?=))))

(defun ar-emacs--point-after-equal-skip-p (ms mb _me)
  "Return t if point is after \\=, nil otherwise.  This predicate
is only tested on  \"skip\" action.

See: https://github.com/Fuco1/smartparens/issues/1101"
  (and
   (equal ms ">")
   (save-excursion
     (goto-char mb)
     (sp--looking-back-p "=" 1))))

(use-package web-mode
  ;; Using mmm-mode now and don't need to set it explicitely anymore
  ;; :mode ("\\.jsx\\'" "\\.tsx\\'")
  :hook
  ((web-mode . company-mode)
   (web-mode . smartparens-strict-mode)
   (web-mode . smartparens-mode)
   (web-mode . subword-mode)
   (web-mode . eldoc-mode))
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")))
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-expanding t)
  (web-mode-enable-auto-indentation t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-part-face t)
  (web-mode-enable-block-face t)
  (web-mode-auto-close-style 2)
  (web-mode-auto-quote-style 3 "Enabling auto quoting for JSX")
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-css-colorization t)
  (company-transformers '(company-sort-by-backend-importance))
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (sp-local-pair 'web-mode "<" ">"
                 :unless '(ar-emacs--point-after-equal-p)
                 :skip-match 'ar-emacs--point-after-equal-skip-p))

;; Multi Major Mode for CSS handling in Typescript/Javascript component files.
;; https://gist.github.com/rangeoshun/67cb17392c523579bc6cbd758b2315c1
(use-package mmm-mode
  :custom
  (mmm-global-mode 'buffers-with-submode-classes)
  (mmm-submode-decoration-level 1 "Turn off background highlight")
  (mmm-parse-when-idle t)

  :config
  ;; css-mode for CSS in JS blocks
  (mmm-add-classes
   '((mmm-styled-mode
      :submode css-mode
      :front "\\(styled\\|css\\)[.()<>[:alnum:]]?+`"
      :back "`;")))
  (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-styled-mode)
  ;; web-mode for nested JSX tags
  (mmm-add-classes
   '((mmm-nested-jsx-mode
      :include-front t
      :front "\\(return\s\\|=>\s\\|\n\s\\|\\s(\n\s*\\)<\\([a-zA-Z0-9_-]+\\)"
      :include-back t
      :back "~2>\n*\s*)"
      :save-matches t
      :submode web-mode)))
  (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-nested-jsx-mode)
  ;; web-mode for single line html tags
  ;; (mmm-add-classes
  ;;  '((mmm-single-jsx-mode
  ;;     :front "\\(return\s\\|n\s\\|(\n\s*\\)<"
  ;;     :front-offset -1
  ;;     :back "/>\n*\s*)"
  ;;     :back-offset 1
  ;;     :submode web-mode)))
  ;; (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-single-jsx-mode)
  ;; Add CSS to our custom styled function components
  (mmm-add-classes
   '((mmm-custom-styled-mode
      :submode css-mode
      :front "\\(styled\\|css\\)\\s(\\(.\\|\n\\)*?`"
      :back "`$")))
  (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-custom-styled-mode))

(use-package mmm-auto)

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
