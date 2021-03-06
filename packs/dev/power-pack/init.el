;;; init.el --- Live Power Pack

;;; Commentary:

;;; Code:

(require 'mwe-log-commands)
(require 'lively)

(live-load-config-file "highlight-tail-conf.el")
(live-load-config-file "expand-region-conf.el")
(live-load-config-file "ace-jump-mode-conf.el")
(live-load-config-file "multiple-cursors-conf.el")
(live-load-config-file "paren-conf.el")
(live-load-config-file "wc-mode-conf.el")
(live-load-config-file "eval-in-repl-conf.el")
(live-load-config-file "zeal-at-point-conf.el")
(live-load-config-file "gtags-conf.el")
(live-load-config-file "treemacs-conf.el")

(use-package git-gutter
  :commands global-git-gutter-mode
  :custom
  (git-gutter:window-width 2)
  (git-gutter:lighter " Gut")
  (git-gutter:modified-sign "~ ")
  (git-gutter:added-sign "+ ")
  (git-gutter:deleted-sign "- ")
  (git-gutter:unchanged-sign "  ")
  (git-gutter:disabled-modes (list 'org-mode)))

(global-git-gutter-mode t)

(use-package docker
  :commands (docker)
  :bind ("C-c x d" . docker))

(use-package synosaurus
  :bind (:map text-mode-map
         ("C-c s l" . synosaurus-lookup)
         ("C-c s i" . synosaurus-choose-and-insert)
         ("C-c s r" . synosaurus-choose-and-replace))
  :custom
  (synosaurus-backend 'synosaurus-backend-wordnet "Set the wordnet backend."))

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom
  (markdown-open-command "function md() { pandoc \"$1\" | lynx -stdin; }; md" "Render markdown using pandoc."))

(use-package text-mode
 :preface (provide 'text-mode)
 :mode "\\.adoc$"
 :hook
 (text-mode . flyspell-mode)
 (text-mode . synosaurus-mode))

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

(use-package feature-mode
  :mode "\\.feature$"
  :hook (cucumber-mode . yas-minor-mode)
  :custom
  (feature-default-language "en" "Set the default language"))

(use-package perspective
  :after projectile
  :config
  (persp-mode))

(use-package dap-mode
  :after lsp-mode)

(use-package dap-java
  :after (dap-mode lsp-java))

;;; init.el ends here
