;;; cider-conf.el --- Cider Config

;;; Commentary:

;;; Code:

(live-add-pack-lib "cider")

(load "cider-autoloads" t t)

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
  (cider-offer-to-open-cljs-app-in-browser nil "Ask before opening up the browser")
  (cider-invert-insert-eval-p t "Always eval after insert into REPL")
  (cider-switch-to-repl-on-insert nil "Do not switch to the REPL on insert")
  (cider-prompt-for-symbol nil "Do not prompt for symbol (in docs among other things)")
  (cider-ns-refresh-show-log-buffer nil "Do not pop up the cider-ns-refresh logs")

  (nrepl-use-ssh-fallback-for-remote-hosts t "Enabling either of these causes CIDER to use TRAMP for some SSH operations, which parses config files such as ~/.ssh/config and ~/.ssh/known_hosts.")
  (cider-infer-remote-nrepl-ports t "Enabling either of these causes CIDER to use TRAMP for some SSH operations, which parses config files such as ~/.ssh/config and ~/.ssh/known_hosts.")

  (cider-known-endpoints '(("localhost" "1667") ;; babashka
                           ("localhost" "5555") ;; common
                           ))
  ;; (cider-print-options nil "zprint options are chosen from .zprint.edn") ;; see https://github.com/clojure-emacs/cider/issues/2966

  :hook
  (cider-mode . eldoc-mode)
  (cider-mode . which-key-mode)
  (cider-repl-mode . (lambda () (helm-cider-mode 1)))

  (cider-repl-mode . eldoc-mode)
  (cider-repl-mode . subword-mode)
  (cider-repl-mode . paredit-mode)
  (cider-repl-mode . company-mode)
  (cider-repl-mode . which-key-mode)
  (cider-repl-mode . (lambda () (helm-cider-mode 1))))

;;; cider-conf.el ends here
