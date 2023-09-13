;;; init.el --- Git Pack

;;; Commentary:

;;; Code:

(live-load-config-file "git-conf.el")
(live-load-config-file "magit-conf.el")

(use-package git-timemachine
  :custom
  (git-timemachine-abbreviation-length 8)
  :config
  (require 'magit))

;;; init.el ends here
