;;; init.el --- Live Power Pack

;;; Commentary:

;;; Code:

(require 'mwe-log-commands)
(require 'iy-go-to-char)
(require 'lively)

(live-load-config-file "highlight-tail-conf.el")
(live-load-config-file "expand-region-conf.el")
(live-load-config-file "ace-jump-mode-conf.el")
(live-load-config-file "multiple-cursors-conf.el")
(live-load-config-file "paren-conf.el")
(live-load-config-file "git-gutter-conf.el")
(live-load-config-file "wc-mode-conf.el")
(live-load-config-file "eval-in-repl-conf.el")
(live-load-config-file "zeal-at-point-conf.el")

(use-package docker
  :commands (docker)
  :bind ("C-c x d" . docker))

(use-package synosaurus
  :bind (:map text-mode-map
         ("C-c s l" . synosaurus-lookup)
         ("C-c s i" . synosaurus-choose-and-insert)
         ("C-c s r" . synosaurus-choose-and-replace))
  :custom
  (synosaurus-backend 'synosaurus-backend-wordnet "Set the wordnet backend.")
  :hook (text-mode . synosaurus-mode))

(use-package know-your-http-well
  :defer t
  :commands (http-header
             http-method
             http-relation
             http-status-code))

(use-package company-restclient
  :defer t
  :after (company cl-lib restclient know-your-http-well)
  :hook (restclient-mode . (lambda ()
                             (set (make-local-variable 'company-backends)
                                  (list
                                   (cons 'company-restclient default-company-backends))))))

(use-package restclient
  :mode "\\.http$"
  :hook (restclient-mode . company-mode))

;;; init.el ends here
