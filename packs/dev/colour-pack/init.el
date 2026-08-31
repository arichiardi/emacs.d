;;; init.el --- Colour Pack  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(global-hl-line-mode 1)

(live-load-config-file "live-fontify-hex-conf.el")

;; From https://github.com/protesilaos/modus-themes/issues/112#issuecomment-2234622808
;; Match on the THEME argument, NOT `custom-enabled-themes': during
;; `load-theme' these functions also run for the internal `user' theme,
;; at which point `custom-enabled-themes' already lists the target
;; theme -- guarding there re-fires the hook twice.
;; No lambda-themes handling needed: the library registers its own
;; hook via `enable-theme-functions' (Emacs 29+).
(defun ar-emacs--run-theme-hooks (theme)
  "Run theme-specific hooks if THEME is one of the enabled themes."
  (when (memq theme '(modus-vivendi modus-operandi))
    (run-hooks 'modus-themes-after-load-theme-hook)))

(add-hook 'enable-theme-functions #'ar-emacs--run-theme-hooks)

(defun ar-emacs--lambda-theme-customizations ()
  "Apply customizations after lambda theme loads.

The `cider-debug-code-overlay-face' colors are applied with `set-face-*'
after `cider-debug' loads (it is not loaded yet when this hook fires,
and `custom-set-faces' here would break later when CIDER's `defface'
re-evaluates the spec: the `lambda-mild' / `lambda-ultralight' color
variables are lexically scoped to the theme form, so a deferred spec
evaluation sees unbound symbols)."
  (with-eval-after-load 'cider-debug
    (set-face-background 'cider-debug-code-overlay-face
                         (face-foreground 'lambda-mild))
    (set-face-foreground 'cider-debug-code-overlay-face
                         (face-background 'lambda-ultralight))))

(use-package lambda-themes
  :config
  ;; NOTE: register the hook manually instead of via `:hook', because
  ;; use-package's keyword-form `:hook' assumes the package provides a
  ;; `<package>-mode' minor mode (it would autoload `lambda-themes-mode'
  ;; -- which does not exist in current lambda-themes -- and wire it into
  ;; every listed hook, breaking `load-theme').
  (add-hook 'lambda-themes-after-load-theme-hook
            #'ar-emacs--lambda-theme-customizations)
  (setopt lambda-themes-set-theme 'dark-faded)
  (setopt lambda-themes-set-italic-comments nil)
  (setopt lambda-themes-set-italic-keywords nil)
  (setopt lambda-themes-set-variable-pitch nil))

;; https://www.gnu.org/software//emacs/manual/html_node/modus-themes/Sample-configuration-with-and-without-use_002dpackage.html
(use-package emacs
  :config
  (load-theme 'lambda-dark-faded :no-confirm))

;;; init.el ends here
