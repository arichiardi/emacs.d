;;; helm-conf.el --- Helm Config

;;; Commentary:

;;; Code:


(use-package helm-ag
  :after helm
  :custom
  (helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (helm-ag-command-option "--all-text")
  (helm-ag-insert-at-point 'symbol)
  (helm-ag-use-grep-ignore-list t))

;;; helm-conf.el ends here
