;;; init.el --- Git Pack

;;; Commentary:

;;; Code:

(live-load-config-file "git-conf.el")
(live-load-config-file "magit-conf.el")

(use-package git-timemachine
  :commands (magit-blame magit-show-commit))
  :custom
  (git-timemachine-abbreviation-length 8))

;;; init.el ends here
