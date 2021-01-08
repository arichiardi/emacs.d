;;; completion-conf.el --- Completion Config

;;; Commentary:

;;; Code:

(use-package company
  :diminish
  :defer t
  :custom
  (company-idle-delay 0 "Set delay to zero")
  :hook
  ((company-mode . (lambda () (company-quickhelp-mode 1)))
   (shell-mode . (lambda ()
                   (add-to-list (make-local-variable 'company-backends)
                                '(company-shell company-shell-env))))))

;;; completion-conf.el ends here
