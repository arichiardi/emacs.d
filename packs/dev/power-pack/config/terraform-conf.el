;;; terraform-conf.el --- Terraform Config

;;; Commentary:

;;; Code:

(use-package hcl-mode)

(use-package company-terraform)

(use-package terraform-mode
  :mode ("\\.tf$" "\\.tfvars$")
  :hook
  ((terraform-mode . (lambda ()
                        (flyspell-prog-mode)
                        (rainbow-delimiters-mode-enable)
                        (company-mode-on)
                        (company-terraform-init)))))

;;; terraform-conf.el ends here
