;;; spelling-conf.el --- Plantuml Config

;;; Commentary:

;;; Code:

(use-package flyspell
  :config
  (progn
    ;; run flyspell with aspell, not ispell
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra"))
    (setq ispell-list-command "--list"))

  :hook
  (git-commit-mode . flyspell-mode)
  (git-rebase-mode . flyspell-mode)
  (markdown-mode . flyspell-mode))

;;; spelling-conf.el ends here
