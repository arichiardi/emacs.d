;;; cider-conf.el --- Cider Config

;;; Commentary:

;;; Code:

(live-add-pack-lib "sesman")
(live-add-pack-lib "cider")
(live-add-pack-lib "spinner-el")
(live-add-pack-lib "seq-el")
(live-add-pack-lib "parseedn")

(load "cider-autoloads" t t)

;; Known hosts
;; (setq cider-known-endpoints '(("localhost" "5555") ("localhost" "5055") ("localhost" "5088")))

(use-package cider
  ;; This seems enough for cider, see also:
  ;; https://emacs.stackexchange.com/questions/19694/use-package-defer-t-and-autoloads
  :defer t

  :config
  (cider-company-enable-fuzzy-completion)

  :custom
  (cider-popup-stacktraces t "Popup stacktraces always on")
  (cider-popup-stacktraces-in-repl nil "Popup stacktraces not in REPL")
  (cider-repl-use-clojure-font-lock t "Use font-lock")
  (cider-prompt-for-project-on-connect nil "Do not prompt for project")
  (cider-repl-display-help-banner nil "No banner")
  (cider-connection-message-fn 'cider-random-tip "Tips are nice")
  (cider-overlays-use-font-lock t "Use overlays")
  (cider-repl-wrap-history t "Wrap history")
  (cider-repl-history-size 2000 "Custom history size")
  (cider-use-tooltips nil "Do not use tooltips")
  ;; (cider-show-error-buffer 'only-in-repl "Show error buffer only in the REPL")
  (cider-offer-to-open-cljs-app-in-browser nil "Ask before opening up the browser")
  (cider-invert-insert-eval-p t "Enable insert-into-REPL eval behavior")
  (cider-switch-to-repl-on-insert nil "Do not switch to the REPL on insert")
  (cider-prompt-for-symbol nil "Do not prompt for symbol (in docs among other things)")

  :hook
  (cider-mode . eldoc-mode)

  (cider-repl-mode . eldoc-mode)
  (cider-repl-mode . subword-mode)
  (cider-repl-mode . paredit-mode)
  (cider-repl-mode . company-mode)
  )

;;; cider-conf.el ends here
