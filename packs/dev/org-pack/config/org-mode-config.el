;; org mode config

(live-add-pack-lib "org-mode/lisp")
(live-add-pack-lib "org-mode/contrib/lisp")

;; set ODT data directory to emacs-live's org-mode
(setq org-odt-data-dir (expand-file-name "./org-mode/etc" (live-pack-lib-dir)))

;; Fix conflicts (http://orgmode.org/org.html#Conflicts)

;; windmove compatibility
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(eval-after-load "org"
  '(require 'ox-md nil t))

(require 'org)
