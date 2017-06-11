;;; git-pack/magit-conf.el
(live-add-pack-lib "with-editor")
(live-add-pack-lib "magit/lisp")

(require 'magit)
(when (not (require 'magit-version nil t))
  (message "`magit-version' not found, try to run make in magit/lisp"))

(add-hook 'magit-log-edit-mode-hook
          (lambda ()
             (set-fill-column 72)
             (auto-fill-mode 1)))

(defvar live-magit-documentation-dir (expand-file-name (concat (live-pack-lib-dir) "magit/Documentation")))

(message (concat "Magit's Documentation: " live-magit-documentation-dir) )

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list live-magit-documentation-dir))
