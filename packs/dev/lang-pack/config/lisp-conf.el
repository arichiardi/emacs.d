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
         :map sly-editing-mode-map
         ("C-M-x" . sly-compile-defun)
         ("C-c C-c" . sly-eval-last-expression))
  :hook (sly-mode . ar-emacs--configure-common-lisp)
  :custom
  (sly-net-coding-system 'utf-8-unix "Default coding system utf8")
  (sly-kill-without-query-p t "Do not ask before killing")
  (sly-lisp-implementations `((sbcl ("sbcl" "--noinform")
                                    :coding-system utf-8-unix
                                    :env (,(concat "SBCL_HOME=" (expand-file-name ".local/lib/sbcl" "~")))))
                            "Set sly lisp implementations")
  (sly-default-lisp 'sbcl "Set default implementation to Steel Bank Common Lisp"))

(use-package sly-mrepl
  :bind (:map sly-mrepl-mode-map
         ("C-<up>" . sly-mrepl-previous-input-or-button)
         ("C-<down>" . sly-mrepl-next-input-or-button)
         ("C-r" . comint-history-isearch-backward)
         ("C-M-r" . comint-history-isearch-backward-regexp)))

(use-package sly-quicklisp
  :defer t
  :after (sly))

(use-package sly-asdf
  :demand t
  :after (sly)
  ;; sly already does this for us
  ;; :config
  ;; (push 'sly-asdf sly-contribs)
  )

(use-package sly-repl-ansi-color
  :demand t
  :after (sly)
  :config
  (push 'sly-repl-ansi-color sly-contribs))


;;; lisp-conf.el ends here
