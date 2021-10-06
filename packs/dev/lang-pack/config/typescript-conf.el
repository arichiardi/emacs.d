;;; typescript-conf.el --- Java Config

;;; Commentary:

;;; Code:

(live-add-pack-lib "smartparens")
(live-add-pack-lib "tide")
(live-add-pack-lib "typescript.el")

;; Largely inspired by https://pastebin.com/hmezXa2e

(defun ar-emacs--setup-tsx-company-backends ()
  "Set up the company-backends for tsx files."
  (make-local-variable 'company-backends)
  (setq company-backends
        '((company-tide company-files :with company-yasnippet :with company-dabbrev-code)
          (company-dabbrev-code company-dabbrev))))

(defun ar-emacs--tsserver-from-node-modules ()
  "Use your node_modules's TSServer to avoid possible version mismatches.

Put this in .dir-locals.el:

  ((typescript-mode . ((eval . (setq tide-tsserver-executable (ar-emacs--tsserver-from-node-modules))))))
"
  (let* ((node-module-path "node_modules/.bin/tsserver")
         (root (projectile-project-root)))
    (expand-file-name node-module-path root)))

(defun ar-emacs--tslint-from-node-modules ()
  "Use your node_modules's TSServer to avoid possible version mismatches.

Put this in .dir-locals.el:

  ((typescript-mode . ((eval . (setq flycheck-typescript-tslint-executable (ar-emacs--tslint-from-node-modules))))))
"
  (let* ((node-module-path "node_modules/.bin/tslint")
         (root (projectile-project-root)))
    (expand-file-name node-module-path root)))

(use-package typescript-mode
  :after (mmm-mode)
  :mode ("\\.[jt]sx?\\'")
  :init
  (require 'smartparens-javascript)
  :hook
  ((typescript-mode . company-mode)
   (typescript-mode . smartparens-strict-mode)
   (typescript-mode . subword-mode)
   (typescript-mode . eldoc-mode)
   (typescript-mode . which-key-mode)
   (typescript-mode . tide-setup)
   (typescript-mode . tide-hl-identifier-mode)
   (typescript-mode . ar-emacs--setup-tsx-company-backends)
   ;; Does not inherit from prog-mode
   (typescript-mode . hl-todo-mode))
  :custom
  (tide-sync-request-timeout 3 "Seems like two seconds is too little sometimes")
  (typescript-indent-level 2)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append))

(use-package tide
  :init
  (setq tide-tsserver-logs-folder (expand-file-name "tsserver" live-tmp-dir))
  (setq tide-tsserver-process-environment (format "TSS_LOG=-level verbose -file %s" tide-tsserver-logs-folder)))

;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; From Spacemacs
(defun ts-open-region-in-playground (start end)
  "Open selected START to END characters (region) in the TypeScript Playground.

If nothing is selected - open the whole current buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (browse-url (concat "http://www.typescriptlang.org/Playground#src="
                      (url-hexify-string (buffer-substring-no-properties start end)))))

;;; typescript-conf.el ends here
