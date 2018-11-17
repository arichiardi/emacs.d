;;; magit-conf.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the magit package

;;; Code:

;; (live-add-pack-lib "with-editor")
;; (live-add-pack-lib "magit-popup")
;; (live-add-pack-lib "ghub")
;; (live-add-pack-lib "magit/lisp")

(use-package magit
  :defer t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :init
  (setq magit-view-git-manual-method 'woman)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))


;; (defvar live-magit-lisp-dir (expand-file-name (concat (live-pack-lib-dir) "magit/lisp")))
;; (defvar live-magit-documentation-dir (expand-file-name (concat (live-pack-lib-dir) "magit/Documentation/")))
;; (defvar live-magit-autoloads (concat live-magit-lisp-dir "/magit-autoloads.el"))

(add-hook 'magit-log-edit-mode-hook
          (lambda ()
             (set-fill-column 72)
             (auto-fill-mode 1)))

;; (when (not (file-exists-p live-magit-autoloads))
  ;; (live-compilation-warning (concat "Cannot find: " live-magit-autoloads)))

;; (load live-magit-autoloads)

;; (require 'magit-version)
;; (message (concat "Magit version: " (magit-version)))

;; (with-eval-after-load 'info
  ;; (info-initialize)
  ;; (add-to-list 'Info-directory-list live-magit-documentation-dir))

;; Local Variables:
;; indent-tabs-mode: nil

;; End:
;;; magit-conf.el ends here
