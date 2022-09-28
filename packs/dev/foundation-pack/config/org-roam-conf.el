;;; org-roam-conf.el --- Org Roam Config

;;; Commentary:
;;
;; See manual https://www.orgroam.com/manual.html
;;
;;; Code:

(use-package org-roam
  :after org
  :config
  (setq org-roam-directory (concat ar-emacs-org-directory "/zettelkasten"))
  (setq org-roam-db-location (expand-file-name "org-roam.db" live-etc-dir))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-database-connector 'sqlite)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)))

(use-package org-roam-ui
  :after org
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;;; org-roam-conf.el ends here
