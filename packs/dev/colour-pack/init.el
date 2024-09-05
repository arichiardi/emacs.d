;;; init.el --- Colour Pack

;;; Commentary:

;;; Code:

(global-hl-line-mode 1)

(live-load-config-file "live-fontify-hex-conf.el")

(defun my-modus-themes-custom-faces (&rest _)
  (modus-themes-with-colors
    (custom-set-faces
     `(helm-rg-title-face ((,c :inherit :background ,bg-heading-1 :foreground ,fg-heading-1)))
     `(helm-rg-extra-arg-face ((,c :inherit :foreground ,yellow-faint)))
     `(helm-rg-active-arg-face ((,c :inherit :foreground ,green-faint)))
     `(helm-rg-error-message ((,c :inherit :foreground ,modeline-err)))
     `(helm-rg-title-face ((,c :inherit :foreground ,magenta-faint)))
     `(helm-rg-directory-header-face ((,c :inherit :background ,bg-main :foreground ,fg-main))))))

(use-package modus-vivendi-theme
  :hook (modus-themes-after-load-theme . #'my-modus-themes-custom-faces)
  :config (load-theme 'modus-vivendi t))

;;; init.el ends here
