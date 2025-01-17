;;; flycheck-conf.el --- Flycheck Config

;;; Commentary:

;;; Code:

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-posframe
  :functions flycheck-posframe-configure-pretty-defaults
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

;;; flycheck-conf.el ends here
