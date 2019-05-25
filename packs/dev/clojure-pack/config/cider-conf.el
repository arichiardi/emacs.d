(live-add-pack-lib "sesman")
(live-add-pack-lib "cider")
(live-add-pack-lib "spinner-el")
(live-add-pack-lib "seq-el")
(live-add-pack-lib "parseedn")

(load "cider-autoloads" t t)

(use-package cider
  ;; This seems enough for cider, see also:
  ;; https://emacs.stackexchange.com/questions/19694/use-package-defer-t-and-autoloads
  :defer t

  :init
  (setq cider-popup-stacktraces t
        cider-popup-stacktraces-in-repl nil
        cider-repl-use-clojure-font-lock t
        cider-prompt-for-project-on-connect nil
        cider-repl-display-help-banner nil
        cider-connection-message-fn 'cider-random-tip
        cider-overlays-use-font-lock t
        cider-repl-wrap-history t
        cider-repl-history-size 1000
        cider-use-tooltips nil
        cider-show-error-buffer 'only-in-repl
        cider-offer-to-open-cljs-app-in-browser nil
        cider-invert-insert-eval-p t
        cider-switch-to-repl-on-insert nil
 )

  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'subword-mode)
  (add-hook 'cider-mode-hook #'paredit-mode)
  (add-hook 'cider-mode-hook #'company-mode)

  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode))
