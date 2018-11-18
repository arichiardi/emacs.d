(live-add-pack-lib "sesman")
(live-add-pack-lib "cider")
(live-add-pack-lib "spinner-el")
(live-add-pack-lib "seq-el")

(use-package cider
  :defer t
  :commands (cider-connect-clj
             cider-connect-cljs
             cider-jack-in-clj
             cider-jack-in-cljs)

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


;; Pull in the awesome clj-refactor lib by magnars
(live-add-pack-lib "jump-el")
(live-add-pack-lib "hydra")
(live-add-pack-lib "clj-refactor")

(use-package clj-refactor
  :config
  (add-hook 'cider-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-m"))))
