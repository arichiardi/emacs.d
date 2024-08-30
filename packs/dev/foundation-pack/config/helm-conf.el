;;; helm-conf.el --- Helm Config
;;
;;; Commentary:
;;
;; Originally from (Thank you Thierry):
;;
;; https://github.com/thierryvolpiatto/emacs-config/blob/main/init-helm.el
;;
;; Some configuration for Helm following this guide:
;; http://tuhdo.github.io/helm-intro.html

;;; Code:

(use-package helm
  :diminish helm-mode
  :config
  ;; https://github.com/syl20bnr/spacemacs/issues/13564
  ;; another useful command is (delete-dups extended-command-history)
  (setq history-delete-duplicates t)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-info-bash)
  (helm-mode 1)

  :custom
  ;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
  ;; discussion of these options.
  (helm-split-window-in-side-p t)
  (helm-buffers-fuzzy-matching t)
  (helm-move-to-line-cycle-in-source t)
  (helm-ff-search-library-in-sexp t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-commands-using-frame '(completion-at-point
                               helm-apropos
                               helm-eshell-prompts
                               helm-imenu
                               helm-imenu-in-all-buffers)))

(use-package helm-info
  :after helm
  :bind (("C-h r" . helm-info-emacs)))

(use-package helm-descbinds
  :after helm
  :config
  ;; C-h b, C-x C-h etc...
  (helm-descbinds-mode 1))

(use-package helm-projectile
  :after helm
  :config
  (helm-projectile-on)
  :custom (helm-projectile-ignore-strategy 'search-tool))

(use-package helm-net
  :after helm
  :config
  (setq helm-net-prefer-curl           t
        helm-surfraw-duckduckgo-url    "https://duckduckgo.com/?q=%s&ke=-1&kf=fw&kl=fr-fr&kr=b&k1=-1&k4=-1"
        helm-google-suggest-search-url helm-surfraw-duckduckgo-url))

(use-package helm-external
  :after helm
  :config
  (setq helm-raise-command                 "wmctrl -xa %s"
        helm-default-external-file-browser "pcmanfm"))

(use-package helm-elisp
  :config
  (setq helm-show-completion-display-function #'helm-display-buffer-in-own-frame
        helm-apropos-fuzzy-match              t
        helm-lisp-fuzzy-completion            t)
  (helm-multi-key-defun helm-multi-lisp-complete-at-point
      "Multi key function for completion in emacs lisp buffers.
First call indent, second complete symbol, third complete fname."
    '(helm-lisp-indent
      helm-lisp-completion-at-point
      helm-complete-file-name-at-point)
    0.3)
  (define-key emacs-lisp-mode-map (kbd "TAB") 'helm-multi-lisp-complete-at-point)
  (define-key lisp-interaction-mode-map (kbd "TAB") 'helm-multi-lisp-complete-at-point))

(use-package helm-org
  :after helm
  :config
  (setq helm-org-headings-fontify t))

(use-package helm-buffers
  :config
  (setq helm-buffers-favorite-modes
        (append helm-buffers-favorite-modes '(picture-mode artist-mode))
        helm-buffers-fuzzy-matching       t
        helm-buffer-skip-remote-checking  t
        helm-buffer-max-length            22
        helm-buffers-end-truncated-string "…"
        helm-buffers-maybe-switch-to-tab  t
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-buffer-not-found)
        helm-boring-buffer-regexp-list
        '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf"
          "\\`\\*Messages" "\\`\\*Magit" "\\`\\*git-gutter" "\\`\\*Help"))

  (define-key helm-buffer-map (kbd "C-d") 'helm-buffer-run-kill-persistent)

  (cl-defmethod helm-setup-user-source ((source helm-source-buffers))
    "Adds additional actions to `helm-source-buffers-list' - Magit."
    (setf (slot-value source 'candidate-number-limit) 300)
    (helm-aif (slot-value source 'action)
        (setf (slot-value source 'action)
              (helm-append-at-nth
               (if (symbolp it)
                   (symbol-value it)
                 it)
               '(("Diff buffers" . helm-buffers-diff-buffers)) 4)))
    (helm-source-add-action-to-source-if
     "Magit status"
     (lambda (candidate)
       (funcall helm-ls-git-status-command
                (with-current-buffer candidate default-directory)))
     source
     (lambda (candidate)
       (locate-dominating-file (with-current-buffer candidate default-directory)
                               ".git"))
     1)))

(use-package helm-cider
  :after cider-mode
  :commands helm-cider-mode
  :bind (:map cider-repl-mode-map
         ("C-r" . helm-cider-repl-history)))

(use-package helm-ag
  :after helm
  :custom
  (helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (helm-ag-insert-at-point 'symbol))

(use-package helm-rg
  :after helm
  :custom
  (helm-rg-input-min-search-chars 3)
  (helm-rg-file-paths-in-matches-behavior 'relative))

(use-package helm-xref
  :after helm)

;;; helm-conf.el ends here
