;;; cider-conf.el --- Cider Config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(load "cider-autoloads" t t)

(use-package clj-refactor
  :diminish clj-refactor-mode
  :config
  (setq cljr-magic-require-namespaces (append '(("edn"   . "clojure.edn")
                                                ("s" . "clojure.spec.alpha")
                                                ("str" . "clojure.string")
                                                ("ig" . "integrant.core"))))
  (setq cljr-project-clean-exceptions (append '("deps.edn" "build.clj")))

  :custom
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (cljr-warn-on-eval nil)
  (cljr-eagerly-build-asts-on-startup t "do not build the project AST on startup")
  (cljr-auto-clean-ns nil "We do not want to mess with existing requires")
  (cljr-auto-sort-ns nil "We do not want to mess with existing requires")
  (cljr-favor-prefix-notation nil "no we do not like it")
  (cljr-clojure-test-declaration "[clojure.test :as test :refer [deftest testing is]]")
  (cljr-magic-requireds t "Never prompt but only complete the requires from cljr-magic-require-namespaces"))

(use-package ob-clojure
  :init
  (setq org-babel-clojure-backend 'cider))

;; From http://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
(defun custom-cider-mode-hook ()
  (let ((oldmap (cdr (assoc 'cider-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)

    (define-key newmap (kbd "C-c M-J") nil)
    (define-key newmap (kbd "C-c M-c") nil)
    (define-key newmap (kbd "C-c M-j") nil)
    (define-key newmap (kbd "C-c C-z") nil)
    (define-key newmap (kbd "C-c C-a") 'cider-switch-to-repl-buffer)

    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(cider-mode . ,newmap) minor-mode-overriding-map-alist)))

(use-package cider
  ;; This seems enough for cider, see also:
  ;; https://emacs.stackexchange.com/questions/19694/use-package-defer-t-and-autoloads
  :defer t

  :config
  (cider-company-enable-fuzzy-completion)

  :bind ((:map cider-ns-map
               ("d" . ar-emacs-cider-repl-in-ns-dev))
         (:map cider-eval-commands-map
               ("M-a" . clojure-align)
               ("M-b" . ar-emacs-clj-eval-all-let-bindings)
               ("M-d" . cider-debug-defun-at-point)
               ("M-s" . clojure-sort-ns)))

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
  (cider-reuse-dead-repls 'auto "do not prompt unless necessary")
  (cider-download-java-sources t "cider 1.17 new feature")

  (nrepl-use-ssh-fallback-for-remote-hosts t "Enabling either of these causes CIDER to use TRAMP for some SSH operations, which parses config files such as ~/.ssh/config and ~/.ssh/known_hosts.")
  (cider-infer-remote-nrepl-ports t "Enabling either of these causes CIDER to use TRAMP for some SSH operations, which parses config files such as ~/.ssh/config and ~/.ssh/known_hosts.")

  (cider-known-endpoints '(("localhost" "1667") ;; babashka
                           ("localhost" "5555") ;; common
                           ))
  ;; Pretty printing
  (cider-print-fn zprint)
  ;; see https://github.com/clojure-emacs/cider/issues/2966
  (cider-print-options nil "zprint options are chosen from .zprint.edn")

  :hook
  (cider-mode . eldoc-mode)
  (cider-mode . clj-refactor-mode)
  (cider-mode . (lambda ()
                  (progn
                    (yas-minor-mode 1))))
  (cider-mode . custom-cider-mode-hook)
  (cider-repl-mode . eldoc-mode)
  (cider-repl-mode . subword-mode)
  (cider-repl-mode . paredit-mode)
  (cider-repl-mode . company-mode))


(with-eval-after-load "cider-mode"
  (define-key cider-mode-map (kbd "C-c C-z") nil)

  ;; Remove because conflicts with clj-refactor
  (define-key cider-mode-map (kbd "C-c C-m") nil)

  ;; Remove because bound by clojure already
  (define-key cider-mode-map (kbd "C-M-i") nil)
  (define-key cider-mode-map (kbd "C-c C-e") 'cider-insert-last-sexp-in-repl)
  (define-key cider-mode-map (kbd "C-M-.") 'ar-emacs-prelude-goto-symbol)
  (define-key cider-mode-map (kbd "C-s-r") 'ar-emacs-cider-repl-refresh-and-test)

  (define-key cider-mode-map (kbd "C-c r f") 'ar-emacs-cljs-figwheel-main-repl)
  (define-key cider-mode-map (kbd "C-c r n") 'ar-emacs-cljs-node-repl)
  (define-key cider-mode-map (kbd "C-c r b") 'ar-emacs-cljs-boot-repl)
  (define-key cider-mode-map (kbd "C-c r s") 'ar-emacs-cljs-shadow-select-repl))

(with-eval-after-load "cider-repl"
    ;; Remove because conflicts with clj-refactor
  (define-key cider-mode-map (kbd "C-c C-m") nil)

  (define-key cider-repl-mode-map (kbd "C-c r f") 'ar-emacs-cljs-figwheel-main-repl)
  (define-key cider-repl-mode-map (kbd "C-c r n") 'ar-emacs-cljs-node-repl)
  (define-key cider-repl-mode-map (kbd "C-c r p") 'ar-emacs-cljs-piggieback-node-repl)
  (define-key cider-repl-mode-map (kbd "C-c r b") 'ar-emacs-cljs-boot-repl)
  (define-key cider-repl-mode-map (kbd "C-c r s") 'ar-emacs-cljs-shadow-select-repl)

  (define-key cider-repl-mode-map (kbd "C-r") nil))

;;; cider-conf.el ends here
