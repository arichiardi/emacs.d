;;; init.el --- Colour Pack

;;; Commentary:

;;; Code:

(global-hl-line-mode 1)

(live-load-config-file "live-fontify-hex-conf.el")

;; Theme-agnostic setup can be configured thus, from:
;; https://protesilaos.com/emacs/modus-themes#h:86f6906b-f090-46cc-9816-1fe8aeb38776

;; (defvar ar-emacs-after-enable-theme-hook nil
;;    "Normal hook run after enabling a theme.")

;; (defun run-after-enable-theme-hook (&rest _args)
;;    "Run `after-enable-theme-hook'."
;;    (run-hooks 'ar-emacs-after-enable-theme-hook))

;; (advice-add 'enable-theme :after #'run-after-enable-theme-hook)

;; From https://github.com/protesilaos/modus-themes/issues/112#issuecomment-2234622808
(defun ar-emacs--maybe-run-modus-theme-hooks (theme)
  (when (memq theme '(modus-vivendi modus-operandi))
    (run-hooks 'modus-themes-after-load-theme-hook)))
(add-hook 'enable-theme-functions #'ar-emacs--maybe-run-modus-theme-hooks)

(use-package modus-themes
  :demand
  :config
  (load-theme 'modus-vivendi :no-confirm)
  (enable-theme 'modus-vivendi))

;;; init.el ends here
