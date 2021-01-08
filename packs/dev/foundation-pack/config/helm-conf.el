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

(require 'helm-config)

(use-package helm
  :diminish helm-mode
  :requires helm-config
  :config
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-info-bash)
  (helm-mode 1)

  :hook
  ;; use helm to list eshell history
  (eshell-mode . (lambda ()
                         (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))
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
  (helm-projectile-on))

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
  :bind (:map cider-repl-mode-map
         ("C-r" . helm-cider-repl-history)))

(use-package helm-ag
  :after helm
  :custom
  (helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (helm-ag-command-option "--all-text")
  (helm-ag-insert-at-point 'symbol)
  (helm-ag-use-grep-ignore-list t))

;;; Global-map
;;
;;
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-x f")                        'helm-recentf)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key [remap bookmark-jump]                'helm-filtered-bookmarks)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
(global-set-key (kbd "C-h d")                        'helm-info-at-point)
(global-set-key (kbd "C-h i")                        'helm-info)
(global-set-key (kbd "C-h a")                        'helm-apropos)
(global-set-key (kbd "C-h C-d")                      'helm-debug-open-last-log)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "S-<f2>")                       'helm-execute-kmacro)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-mini)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
(global-set-key (kbd "M-s")                          'helm-do-grep-ag)
(global-set-key (kbd "C-x r p")                      'helm-projects-history)
(global-set-key (kbd "C-x r c")                      'helm-addressbook-bookmarks)
(global-set-key (kbd "C-c t r")                      'helm-dictionary)
(global-set-key (kbd "C-x C-b")                      'helm-buffers-list)
(global-set-key (kbd "C-h C-l")                      'helm-locate-library)

;;; helm-conf.el ends here
