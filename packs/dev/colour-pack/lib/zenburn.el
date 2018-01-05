;; zenburn-emacs theme

(live-add-pack-lib "zenburn-emacs")

(defun color-theme-zenburn ()
  "Zenburn theme from zenburn-emacs"
  (interactive)
  (require 'zenburn-theme)
  (load-theme 'zenburn t)
  ;; Fixes the selection background color
  (set-face-attribute 'region nil :background "#666")
  ;; Apparently the zenburn-emacs definitions are not enough
  (eval-after-load 'company-quickhelp
    '(progn
       (setq company-quickhelp-color-background (cdr (assoc "zenburn-bg+1" zenburn-default-colors-alist)))
       (setq company-quickhelp-color-foreground (cdr (assoc "zenburn-fg" zenburn-default-colors-alist))))))
