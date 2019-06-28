(require 'use-package)

(load
 (thread-first
     (file-name-as-directory borg-drone-directory)
   (concat "geiser")
   (file-name-as-directory)
   (concat "build")
   (file-name-as-directory)
   (concat "elisp")
   (file-name-as-directory)
   (concat "geiser-load")))

(defun scheme-conf--geiser-mode-hook ()
  "Geiser mode hook."

  (enable-paredit-mode)
  (company-mode-on)
  (eldoc-mode t)
  (subword-mode t)
  (flyspell-prog-mode)
  (geiser-autodoc-mode t)
  (rainbow-delimiters-mode-enable))

(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))

(use-package geiser
  :init
  (setq geiser-implementations-alist
        '(((regexp "\\.scm$") guile)))
  (add-hook 'geiser-mode-hook #'scheme-conf--geiser-mode-hook)
  (add-hook 'geiser-repl-mode-hook #'scheme-conf--geiser-mode-hook))
