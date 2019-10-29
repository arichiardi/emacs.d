;;; lisp-conf.el --- Lisp Config

;;; Commentary:

;;; Code:

(defun ar-emacs--configure-common-lisp ()
  "Racket mode hook."
  (enable-paredit-mode)
  (company-mode-on)
  (eldoc-mode t)
  (subword-mode t)
  (flyspell-prog-mode)
  (rainbow-delimiters-mode-enable))

(use-package common-lisp-mode
  :mode "\\.lisp\\'")

(use-package sly
  :bind (("C-c M-j" . sly)
         :map sly-mode-map
         ("C-c M-j" . sly)
         ("C-c C-e" . sly-eval-last-expression))
  :hook (sly-mode . ar-emacs--configure-common-lisp)
  :custom
  (sly-kill-without-query-p t "Do not ask before killing")
  (sly-default-lisp 'sbcl "Set default implementation to Steel Bank Common Lisp")
  :config
  (push '(sbcl  ("sbcl" "--noinform") :coding-system utf-8-unix) sly-lisp-implementations))

(use-package sly-quicklisp
  :defer t
  :after (sly))

(use-package sly-asdf
  :defer t
  :after (sly)
  :config
  (add-to-list 'sly-contribs 'sly-asdf 'append))

;;; lisp-conf.el ends here
