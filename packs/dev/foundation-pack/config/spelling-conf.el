;;; spelling-conf.el --- Plantuml Config

;;; Commentary:

;;; Code:

(use-package flyspell
  :custom
  (ispell-local-dictionary "en_US")
  (ispell-program-name "hunspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  (ispell-list-command "--list")

  :hook
  (git-commit-mode . flyspell-mode)
  (git-rebase-mode . flyspell-mode)
  (markdown-mode . flyspell-mode)
  (plantuml-mode . flyspell-prog-mode))

;;; spelling-conf.el ends here
