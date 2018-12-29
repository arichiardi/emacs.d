(live-add-pack-lib "sesman")
(live-add-pack-lib "cider")
(live-add-pack-lib "spinner-el")
(live-add-pack-lib "seq-el")

(use-package cider
  ;; This seems enough for cider, see also:
  ;; https://emacs.stackexchange.com/questions/19694/use-package-defer-t-and-autoloads
  :defer t

  :init
  (setq cider-popup-stacktraces t
        cider-popup-stacktraces-in-repl nil
        cider-repl-use-clojure-font-lock t
        cider-overlays-use-font-lock t
        cider-repl-wrap-history t
        cider-repl-history-size 1000
        cider-show-error-buffer t)

  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode))
