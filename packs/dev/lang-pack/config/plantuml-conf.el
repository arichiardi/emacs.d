(use-package flycheck-plantuml
  :no-require t
  :after (:all flycheck plantuml-mode)
  :config
  (flycheck-plantuml-setup))

(use-package plantuml-mode
  :commands (plantuml-mode org-src-lang-modes)
  :config
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))
