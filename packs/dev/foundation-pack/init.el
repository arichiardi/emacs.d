;;; init.el --- Foundation Pack -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; GnuPG Pinentry
;; https://www.gnu.org/software/emacs/manual/html_node/epa/GnuPG-Pinentry.html
(use-package epg
  :config
  (setopt epg-pinentry-mode 'loopback))

(setq ispell-local-dictionary "en_US-w-accents")

(setq make-backup-files nil) ;; stop creating backup~ files
(setq auto-save-default nil) ;; stop creating #autosave# files
(setq create-lockfiles nil)  ;; stop having lockfiles

(setq live-disable-zone t)
(setq shift-select-mode t)
(setq mouse-drag-copy-region t)
(setq visible-bell t)

(setq auth-sources '("~/.authinfo.gpg"))

(use-package dash
  :config (dash-enable-font-lock))

(use-package sh-mode
  :mode ("\\.bats\\'" "\\.bashrc")
  :hook ((sh-mode . company-mode)
         (sh-mode . rainbow-delimiters-mode)))

(require 'smooth-scrolling)
(require 'buffer-move)

(live-add-pack-lib "s")
(require 's)
(live-add-pack-lib "epl")
(require 'epl)
(live-add-pack-lib "pkg-info")
(require 'pkg-info)

(live-add-pack-lib "ar-emacs")
(require 'ar-emacs)

(live-load-config-file "backup-dir-conf.el")
(live-load-config-file "util-fns.el")
(live-load-config-file "built-in.el")
(live-load-config-file "cua-conf.el")
(live-load-config-file "cosmetic.el")
(live-load-config-file "tramp-conf.el")
(live-load-config-file "mouse-conf.el")
(live-load-config-file "key-chord-conf.el")
(live-load-config-file "recentf-conf.el")
(live-load-config-file "popwin-conf.el")
(live-load-config-file "term-conf.el")
(live-load-config-file "spelling-conf.el")
(live-load-config-file "win-switch-conf.el")
(live-load-config-file "monkey-patch.el")
(live-load-config-file "org-mode-conf.el")
(live-load-config-file "completion-conf.el")
(live-load-config-file "projectile-conf.el")
(live-load-config-file "ediff-conf.el")
(live-load-config-file "lsp-conf.el")
(live-load-config-file "paredit-conf.el")
(live-load-config-file "phi-search-conf.el")
(live-load-config-file "live.el")

(when (eq system-type 'darwin)
  (live-load-config-file "osx.el"))

(use-package transient
  :config
  (setq transient-history-file (expand-file-name (expand-file-name "history.el" "transient")
                                                 live-tmp-dir)))

(use-package ws-butler
  :diminish)

(ws-butler-global-mode)

(use-package editorconfig
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode "Only trim touched lines")
  :hook
  (editorconfig-after-apply-functions . (lambda (_) (setq web-mode-block-padding 0)))
  :config
  (editorconfig-mode 1))

(setq live-exec-path-default-variables '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "PATH" "MANPATH"))
(setq live-exec-path-asdf-variables    '("ASDF_DIR" "ASDF_DATA_DIR"))

(setq-default exec-path-from-shell-variables '())

;; We cannot achieve full speed because of the dynamic nature of asdf.
;;   https://github.com/purcell/exec-path-from-shell?tab=readme-ov-file#setting-up-your-shell-startup-files-correctly
;;
;; Why do we run exec-path-from-shell-initialize twice?
;;
;; The first time sets PATH for things like executable-find, see:
;;   https://github.com/cosmicexplorer/helm-rg/issues/17#issuecomment-2392418383
;;
;; The second time is for allowing packs to customize exec-path-from-shell-variables (packs are
;; loaded last).
(use-package exec-path-from-shell
  :defines (exec-path-from-shell-variables)
  ;; We want to always add the default variables above.
  :init
  (setq exec-path-from-shell-arguments nil)
  (setq exec-path-from-shell-variables
        (-distinct (-non-nil (append exec-path-from-shell-variables live-exec-path-default-variables))))
  :config
  (progn (message "Running exec-path-from-shell.")
         (message "Variables %s" exec-path-from-shell-variables)
         (exec-path-from-shell-initialize))
  :hook
  (after-init . (lambda ()
                  (message "Running exec-path-from-shell after-init.")
                  (message "Variables %s" exec-path-from-shell-variables)
                  (exec-path-from-shell-initialize))))

(use-package string-edit)

(use-package simpleclip
  :commands (simpleclip-get-contents simpleclip-set-contents))

;;;;;;;; From the Vertico configuration

;; Disable annoying tooltips on hover tooltip
(setq show-help-function nil)

;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer to switch display
;; modes.
(setq context-menu-mode t)

;; Support opening new minibuffers from inside existing minibuffers.
(setq enable-recursive-minibuffers t)

;; Hide commands in M-x which do not work in the current mode.  Vertico
;; commands are hidden in normal buffers. This setting is useful beyond
;; Vertico.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
 '(read-only t cursor-intangible t face minibuffer-prompt))

;;; init.el ends here
