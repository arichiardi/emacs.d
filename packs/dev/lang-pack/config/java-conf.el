;;; java-conf.el --- Java Config

;;; Commentary:

;;; Code:

(defun ar-emacs--configure-java ()
  "Java mode hook."
  (when (derived-mode-p 'java-mode)
    (flycheck-mode 1)
    (yas-minor-mode)
    (yas-reload-all)
    (subword-mode 1)
    (smartparens-strict-mode)
    ;; Remapping a couple of commands
    (define-key (current-global-map) [remap ggtags-visit-project-root] #'lsp)
    (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)))

(add-hook 'java-mode-hook #'ar-emacs--configure-java)

(use-package lsp-java
  :after lsp-mode
  :config
  ;; (put 'lsp-java-workspace-dir 'safe-local-variable #'stringp)
  (setq lsp-java-workspace-dir (expand-file-name "workspace" live-etc-dir))
  (setq lsp-java-workspace-cache-dir (expand-file-name ".cache/" lsp-java-workspace-dir))
  (setq lsp-java-server-install-dir (expand-file-name "eclipse.jdt.ls/" lsp-server-install-dir)))

(use-package java-snippets)

(use-package gradle-mode
  :mode "\\.gradle\\'")

(use-package groovy-mode
  :mode ("\\.groovy\\'" "\\.gradle\\'"))

;;; java-conf.el ends here
