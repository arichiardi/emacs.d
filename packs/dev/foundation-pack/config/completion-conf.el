;;; completion-conf.el --- Completion Config

;;; Commentary:

;;; Code:

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setopt completion-styles '(orderless basic))))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package company
  :diminish
  :defer t
  :init
  (setq company-require-match nil)
  (setq company-auto-commit nil)
  :bind (:map company-mode-map
         ([remap completion-at-point] . consult-company))

  :custom
  (company-tooltip-limit 25)
  (company-auto-commit-chars nil)
  (company-tooltip-flip-when-above nil "don't want to press down for going up!")
  (company-idle-delay .2)
  (company-tooltip-idle-delay 0)

  :hook
  (shell-mode . (lambda ()
                  (add-to-list (make-local-variable 'company-backends)
                               '(company-shell company-shell-env)))))

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ;; ("C-c M-x" . consult-mode-command)
         ;; ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ;; ("C-c m" . consult-man)
         ;; ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)

         ;; C-x bindings in `ctl-x-map'
         ("C-x f" . consult-recent-file)       ;; orig. helm-recentf
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer

         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)

         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop

         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g o" . consult-org-heading)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line)            ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)  ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; ‘consult-completion-in-region’: In case you don’t use Corfu as your in-buffer completion UI,
  ;; this function can be set as ‘completion-in-region-function’. Then your minibuffer completion UI
  ;; (e.g., Vertico or Icomplete) will be used for ‘completion-at-point’.
  (setq completion-in-region-function #'consult-completion-in-region)

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '("S-<up>" "S-<down>"))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

)

(use-package consult-ag)

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))


(eval-and-compile
  (defun vertico-load-path ()
    (mapcar (lambda (x)
              (expand-file-name x (car (borg-load-path "vertico"))))
            '("vertico.el" "extensions"))))

(use-package vertico
  :load-path (lambda () (vertico-load-path))
  :init
  (vertico-mode)

  :bind (:map vertico-map
         ("?" . minibuffer-completion-help)
         ("TAB" . vertico-insert)
         ("S-<up>" . vertico-previous)
         ("S-<down>" . vertico-next)
         ("<right>" . vertico-insert))

  :config
  (when (< emacs-major-version 31)
    (advice-add #'completing-read-multiple :filter-args
                (lambda (args)
                  (cons (format "[CRM%s] %s"
                                (string-replace "[ \t]*" "" crm-separator)
                                (car args))
                        (cdr args)))))

  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 10)
  (vertico-resize t)
  (vertico-cycle t "Enable cycling for `vertico-next/previous'"))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("<right>" . vertico-insert)
              ("<left>" . vertico-directory-delete-word)
              ("M-DEL" . vertico-directory-up))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;;; completion-conf.el ends here
