;;; lisp-conf.el --- Lisp Config

;;; Commentary:

;;; Code:

(use-package common-lisp-mode
  :mode "\\.lisp\\'")

(use-package sly
  :bind (("C-c M-j" . sly)
         :map sly-mode-map
         ("C-c M-j" . sly))
  :custom
  (sly-kill-without-query-p t "Do not ask before killing")
  (sly-default-lisp 'sbcl "Set default implementation to Steel Bank Common Lisp")
  :config
  (push '(sbcl  ("sbcl") :coding-system utf-8-unix) sly-lisp-implementations))

(use-package sly-quicklisp
  :defer t
  :after (sly))

(use-package sly-asdf
  :defer t
  :after (sly)
  :config
  (add-to-list 'sly-contribs 'sly-asdf 'append))

;;; lisp-conf.el ends here
