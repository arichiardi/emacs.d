;;; git-pack/magit-conf.el
(live-add-pack-lib "with-editor")
(live-add-pack-lib "magit-popup")
(live-add-pack-lib "ghub")
(live-add-pack-lib "magit/lisp")

(defvar live-magit-lisp-dir (expand-file-name (concat (live-pack-lib-dir) "magit/lisp")))
(defvar live-magit-documentation-dir (expand-file-name (concat (live-pack-lib-dir) "magit/Documentation/")))

(add-hook 'magit-log-edit-mode-hook
          (lambda ()
             (set-fill-column 72)
             (auto-fill-mode 1)))


(load (concat live-magit-lisp-dir "/magit-autoloads"))

(message (concat "Magit loaded: " (magit-version)))

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list live-magit-documentation-dir))

(setq magit-view-git-manual-method 'woman)
