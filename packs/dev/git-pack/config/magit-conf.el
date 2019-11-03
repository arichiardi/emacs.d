;;; magit-conf.el --- Magit config
;;; Commentary:
;;
;; Configure the magit package

;;; Code:

(defvar live-magit-dir (expand-file-name "magit" borg-drone-directory))
(defvar live-magit-lisp-dir (expand-file-name "lisp" live-magit-dir))
(defvar live-magit-documentation-dir (expand-file-name "Documentation" live-magit-dir))
;; (defvar live-magit-autoloads (concat live-magit-lisp-dir "/magit-autoloads.el"))

(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :init
  (setq magit-view-git-manual-method 'woman)

  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append)

  :hook
  ((magit-log-edit-mode . (lambda ()
                              (set-fill-column 72)
                              (auto-fill-mode 1)))))

(with-eval-after-load 'info
  (info-initialize)
  (push live-magit-documentation-dir Info-directory-list))

(use-package git-commit
  :custom
  (git-commit-check-style-conventions t "Check for style in commit messages.")
  (git-commit-style-convention-checks '(non-empty-second-line overlong-summary-line) "Set the commit the style we want to enforce")

  :hook
  ((git-commit-setup . git-commit-turn-on-auto-fill)
   (git-commit-setup . git-commit-turn-on-flyspell)
   (git-commit-setup . with-editor-usage-message)))

;; End:
;;; magit-conf.el ends here
