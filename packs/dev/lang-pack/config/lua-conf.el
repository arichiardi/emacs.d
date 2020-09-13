;;; lua-conf.el --- Lua Config

;;; Commentary:

;;; Code:

(use-package lua-mode
  :mode "\\.lua\\'"
  :hook
  ((lua-mode . subword-mode)
   (lua-mode . eldoc-mode)
   (lua-mode . company-mode-on)))

;;; lua-conf.el ends here
