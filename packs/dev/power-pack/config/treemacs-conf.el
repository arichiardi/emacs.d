;;; treemacs-conf.el --- Treemacs Config

;;; Commentary:

;;; Code:

(live-add-pack-lib "treemacs/src/elisp")
(live-add-pack-lib "treemacs/src/extra")

;; Thank you dakra
;; https://github.com/dakra/dmacs/blob/master/init.org#treemacs-a-tree-layout-file-explorer
(defun ar--treemacs-toggle-or-select ()
  "Initialize or toggle treemacs.

- If the treemacs window is visible and selected, hide it.
- If the treemacs window is visible select it.
- If a treemacs buffer exists, but is not visible show it.
- If no treemacs buffer exists for the current frame create and show it.
- If the workspace is empty additionally ask for the root path of the first
  project to add."
  (interactive)
  (pcase (treemacs-current-visibility)
    ('visible (if (string-prefix-p treemacs--buffer-name-prefix (buffer-name))
                  (delete-window (treemacs-get-local-window))
                (treemacs--select-visible-window)))
    ('exists  (treemacs-select-window))
    ('none    (treemacs--init))))

(use-package treemacs
  :defer t
  :commands (treemacs treemacs-current-visibility)
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind
  (("C-c x t" . ar--treemacs-toggle-or-select))
  :custom
  (treemacs-collapse-dirs 5)
  (treemacs-follow-after-init t)
  (treemacs-indentation 1)
  (treemacs-no-png-images t))

(use-package treemacs-magit
  :after treemacs)

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-persective ;; treemacs-persective if you use perspective.el vs. persp-mode
  :after (treemacs perspective)  ;; or perspective vs. persp-mode
  :bind (:map treemacs-mode-map
         ("C-t p" . treemacs-projectile))
  :config
  (treemacs-set-scope-type 'Perspectives))

;;; treemacs-conf.el ends here
