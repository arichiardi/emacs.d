;;; gtags-conf.el --- Treemacs Config

;;; Commentary:

;;; Code:

(use-package cc-mode
  :ensure nil
  :hook (prog-mode . (lambda ()
                       (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'go-mode 'rust-mode)
                         (ggtags-mode 1)))))

(use-package ggtags
  :commands ggtags-mode
  :config
  (unbind-key "M-<" ggtags-mode-map)
  (unbind-key "M->" ggtags-mode-map)
  (unbind-key "M-<" ggtags-navigation-map)
  (unbind-key "M->" ggtags-navigation-map)

  (setq ggtags-process-environment '("GTAGSLABEL=universal-ctags")))

;;; gtags-conf.el ends here
