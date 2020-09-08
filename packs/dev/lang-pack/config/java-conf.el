;;; java-conf.el --- Java Config

;;; Commentary:

;;; Code:

(defun ar-emacs--configure-java ()
  "Java mode hook."
  (when (derived-mode-p 'java-mode)
    (flycheck-mode 1)
    (yas-minor-mode)
    (yas-reload-all)
    (dap-auto-configure-mode)
    ;; Remapping a couple of commands
    (define-key (current-global-map) [remap ggtags-visit-project-root] #'lsp)
    ;; (define-key (current-global-map) [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    ;; (define-key (current-global-map) [remap xref-find-references] #'lsp-ui-peek-find-references)
    (define-key (current-global-map) [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
    (define-key smartparens-mode-map [remap sp-forward-slurp-sexp] #'sp-slurp-hybrid-sexp)

    ;; We want to use Test as test suffix
    (setq projectile-project-types (assq-delete-all 'gradlew projectile-project-types))
    (projectile-register-project-type 'gradlew '("gradlew")
                                      :compile "./gradlew build"
                                      :test "./gradlew test"
                                      :test-suffix "Test")))

(add-hook 'java-mode-hook #'ar-emacs--configure-java)
(add-hook 'java-mode-hook #'editorconfig-mode)
(add-hook 'java-mode-hook #'subword-mode)
(add-hook 'java-mode-hook #'smartparens-strict-mode)

(setq ar-emacs--project-lombok-jar
      (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.12/lombok-1.18.12.jar"))

(use-package lsp-java
  :after lsp-mode
  :config
  (put 'lsp-java-workspace-dir 'safe-local-variable #'stringp)
  (setq lsp-java-workspace-dir (expand-file-name "workspace" live-etc-dir))
  (setq lsp-java-workspace-cache-dir (expand-file-name ".cache/" lsp-java-workspace-dir))
  (setq lsp-java-server-install-dir (expand-file-name "eclipse.jdt.ls/" lsp-server-install-dir))

  ;; This is global, and I don't like it that much
  (setq lsp-java-vmargs (append lsp-java-vmargs (list (concat "-javaagent:" ar-emacs--project-lombok-jar))))
  (setq lsp-java-import-gradle-jvm-arguments `["-Xmx1G" ,(concat "-javaagent:" ar-emacs--project-lombok-jar)])

  :custom
  (lsp-java-code-generation-generate-comments t "Generate methods with comments"))

(use-package java-snippets)

(use-package gradle-mode
  :mode "\\.gradle\\'")

(use-package groovy-mode
  :mode ("\\.groovy\\'" "\\.gradle\\'"))

;;; java-conf.el ends here
