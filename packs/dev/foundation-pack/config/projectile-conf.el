;;; projectile-conf.el --- Projectile Config

;;; Commentary:

;;; Code:

(use-package projectile
  :init
  (setq projectile-cache-file (concat live-tmp-dir "projectile-cache"))
  (setq projectile-known-projects-file (concat live-tmp-dir "projectile-known-projects.eld"))

  :config
  ;; Pretty standard to have this setup, setting it globally
  (projectile-update-project-type 'clojure-cli
                                  :test-dir "test/"
                                  :src-dir "src/")
  (projectile-register-project-type 'yarn '("package.json")
                                    :compile "yarn install"
                                    :test "yarn test"
                                    :run "yarn start")
  (setq projectile-globally-ignored-directories
        (append '("node_modules" "target" "logs")
                projectile-globally-ignored-directories))

  :custom
  (projectile-mode-line-prefix " Prj")
  (projectile-enable-caching t)
  (projectile-switch-project-action 'projectile-find-file-dwim)
  (projectile-idle-timer-seconds 60)
  (projectile-create-missing-test-files t)
  (projectile-completion-system 'helm)
  (projectile-tags-backend 'ggtags))

(use-package persp-projectile
  :after (projectile perspective))

(projectile-global-mode)

;;; projectile-conf.el ends here
