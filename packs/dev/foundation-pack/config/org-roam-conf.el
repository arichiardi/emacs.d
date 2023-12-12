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
  (setq org-roam-dailies-directory "journal/")
  (setq ar-emacs--org-roam-current-job-file (concat org-roam-directory "/cohesic.org.gpg"))

  (setq org-roam-link-extensions '(".org" ".org.gpg"))
  (setq org-roam-db-location (expand-file-name "org-roam.db" live-etc-dir))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  ;; capture
  (setq ar-emacs--org-roam-capture-todo-header "* TODO %^{Brief Description} %^g")
  (setq ar-emacs--org-roam-capture-work-todo
    (string-join
     (list ar-emacs--org-roam-capture-todo-header
           ":PROPERTIES:"
           ":ID: %(org-id-new)"
           ":END:"
           "%?")
     "\n"))

  (setq org-roam-dailies-capture-templates
        `(("d" "default"
           entry
           "** %<%H:%M %p> %i%?\n"
           :if-new (file+head+olp
                    "%<%Y-%m-%d>.org.gpg"
                    ,(string-join
                      (list ":PROPERTIES:"
                            ":ID: %(org-id-new)"
                            ":END:"
                            "-*- epa-file-encrypt-to: (\"a.richiardi.work@gmail.com\") -*-"
                            "-*- backup-inhibited t; -*-"
                            "#+TITLE: %<%Y-%m-%d>"
                            "#+AUTHOR: Andrea Richiardi"
                            "#+CATEGORY: daily"
                            "#+STARTUP: content indent")
                      "\n")
                    ("Journal"))
           :jump-to-captured t)))

  (setq org-roam-capture-templates
        `(("w" "Work Templates")
	      ("wt" "Work Todo"
           entry
           ,ar-emacs--org-roam-capture-work-todo
           :target (file+olp ,ar-emacs--org-roam-current-job-file ("Tasks"))
           :clock-resume t)))

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
