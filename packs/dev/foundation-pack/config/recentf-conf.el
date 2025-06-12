;;; recentf-conf.el --- Recentf config -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package recentf
  :config
  (setopt recentf-max-menu-items 20
        recentf-max-saved-items 100)
  (recentf-mode t))

;;; recentf-conf.el ends here
