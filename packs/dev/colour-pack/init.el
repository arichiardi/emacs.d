;;; init.el --- Colour Pack

;;; Commentary:

;;; Code:

(global-hl-line-mode 1)

(live-load-config-file "live-fontify-hex-conf.el")

;; From https://github.com/protesilaos/modus-themes/issues/112#issuecomment-2234622808
(defun ar-emacs--run-theme-hooks (theme)
  "Run theme-specific hooks if THEME is one of the enabled themes."
  (when (or (member 'modus-vivendi custom-enabled-themes)
            (member 'modus-operandi custom-enabled-themes))
    (run-hooks 'modus-themes-after-load-theme-hook))
  (when (or (member 'lambda-dark custom-enabled-themes)
            (member 'lambda-dask-faded custom-enabled-themes))
    (run-hooks 'lambda-themes-after-load-theme-hook)))

(add-hook 'enable-theme-functions #'ar-emacs--run-theme-hooks)

;; (use-package modus-themes
;;   :demand
;;   :config
;;   (load-theme 'modus-vivendi :no-confirm)
;;   (enable-theme 'modus-vivendi)
;;   )

(use-package lambda-themes
  :demand
  :custom
  (lambda-themes-set-italic-comments nil)
  (lambda-themes-set-italic-keywords nil)
  (lambda-themes-set-variable-pitch nil)
  :config
  (load-theme 'lambda-dark-faded :no-confirm)
  (enable-theme 'lambda-dark))

;;; init.el ends here
