(use-package sly
  :mode ("\\.lisp\\'" . sly-mode)
  :bind ("C-c M-j" . sly)
  :custom
  (sly-default-lisp 'sbcl "Set default implementation to Steel Bank Common Lisp")
  :config
  (push '(sbcl  ("sbcl") :coding-system utf-8-unix) sly-lisp-implementations))

(use-package sly-quicklisp)
