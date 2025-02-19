;;; init.el --- Live Power Pack

;;; Commentary:

;;; Code:

(require 'lively)

(live-load-config-file "irc-conf.el")
(live-load-config-file "highlight-tail-conf.el")
(live-load-config-file "expand-region-conf.el")
(live-load-config-file "multiple-cursors-conf.el")
(live-load-config-file "paren-conf.el")
(live-load-config-file "wc-mode-conf.el")
(live-load-config-file "eval-in-repl-conf.el")
(live-load-config-file "zeal-at-point-conf.el")
(live-load-config-file "gtags-conf.el")
(live-load-config-file "terraform-conf.el")
(live-load-config-file "sql-conf.el")
(live-load-config-file "llm-conf.el")

(use-package git-gutter
  :commands global-git-gutter-mode
  :custom
  (git-gutter:window-width 2)
  (git-gutter:lighter " GG")
  (git-gutter:modified-sign "~ ")
  (git-gutter:added-sign "+ ")
  (git-gutter:deleted-sign "- ")
  (git-gutter:unchanged-sign "  ")
  (git-gutter:disabled-modes '(org-mode org-roam-db-autosync-mode org-roam-mode)))

(global-git-gutter-mode +1)

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
  (markdown-command '("pandoc" "--from=markdown" "--to=html5") "Compile markdown with pandoc, it seems to render GFM better.")
  (markdown-open-command "code" "Fallback mode opens externally with vscode."))

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

(use-package feature-mode
  :mode "\\.feature$"
  :hook (cucumber-mode . yas-minor-mode)
  :custom
  (feature-default-language "en" "Set the default language"))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :hook (prog-mode . (lambda () (persp-mode 1))))

(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode)
  (text-mode . hl-todo-mode)

  :custom
  (hl-todo-highlight-punctuation ":"))

(use-package alert
  :custom
  (alert-default-style 'libnotify)
  (alert-fade-time 3))

(use-package pomidor
  :bind (("<f12>" . pomidor))
  :custom
  (pomidor-sound-tick nil)
  (pomidor-sound-tack nil)
  (pomidor-play-sound-file
   (lambda (file)
     (start-process "emacs-pomidor-play-sound"
                    nil
                    (if (eq system-type 'darwin) "afplay" "aplay")
                    file))))

(use-package olivetti
  :custom
  (olivetti-style 'fancy)
  (olivetti-body-width 0.75))

(use-package org-present
  :custom
  ;; (org-present-text-scale 2 "The original value of 5 is too much.")
  (org-present-startup-folded t "We want to unfold slides slowly.")
  :hook
  (org-present-mode . (lambda ()
                        (org-display-inline-images)
                        (org-present-hide-cursor)
                        (org-present-read-only)
                        (olivetti-mode 1)))
  (org-present-mode-quit . (lambda ()
                             (olivetti-mode -1)
                             (org-remove-inline-images)
                             (org-present-read-write)
                             (org-present-show-cursor))))

(use-package flycheck-plantuml
  :no-require t
  :after (:all flycheck plantuml-mode)
  :config
  (flycheck-plantuml-setup))

(use-package plantuml-mode
  :mode ("\\.plantuml\\'" "\\.puml\\'")
  :commands (org-src-lang-modes)

  :config
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (setq plantuml-jar-path (ar-emacs-plantuml-jar-path))

  :custom
  ((plantuml-output-type "png" "Set the default output to png")
   (plantuml-default-exec-mode 'executable)))

(use-package mermaid-mode
  :mode "\\.mmd$"
  :custom
  (mermaid-tmp-dir (expand-file-name "mermaid" live-tmp-dir)))

(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;; init.el ends here
