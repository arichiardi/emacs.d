;;; completion-conf.el --- Completion Config

;;; Commentary:

;;; Code:

(use-package company
  :diminish
  :defer t
  :init
  (setq company-require-match nil)
  (setq company-auto-commit nil)
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

;;; completion-conf.el ends here
