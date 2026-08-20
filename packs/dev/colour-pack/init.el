;;; init.el --- Colour Pack  -*- lexical-binding: t; -*-

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
            (member 'lambda-dark-faded custom-enabled-themes))
    (run-hooks 'lambda-themes-after-load-theme-hook)))

(add-hook 'enable-theme-functions #'ar-emacs--run-theme-hooks)

(defun ar-emacs--lambda-theme-customizations ()
  "Apply customizations after lambda theme loads."
  (custom-set-faces
   '(cider-debug-code-overlay-face
     ((t (:background lambda-mild
                      :foreground lambda-ultralight))))))

(use-package lambda-themes
  :hook
  (lambda-themes-after-load-theme . #'ar-emacs--lambda-theme-customizations)

  :config
  (setopt lambda-themes-set-theme 'dark-faded)
  (setopt lambda-themes-set-italic-comments nil)
  (setopt lambda-themes-set-italic-keywords nil)
  (setopt lambda-themes-set-variable-pitch nil))

;; https://www.gnu.org/software//emacs/manual/html_node/modus-themes/Sample-configuration-with-and-without-use_002dpackage.html
(use-package emacs
  :config
  (load-theme 'lambda-dark-faded :no-confirm))

;;; init.el ends here
