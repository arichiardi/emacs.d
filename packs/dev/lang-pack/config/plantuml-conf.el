;;; plantuml-conf.el --- Plantuml Config

;;; Commentary:

;;; Code:

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
  (use-package org
    :config
    (setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
    (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

  :custom
  ((plantuml-output-type "png" "Set the default output to png")
   (plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
   (plantuml-default-exec-mode 'jar)))

;;; plantuml-conf.el ends here
