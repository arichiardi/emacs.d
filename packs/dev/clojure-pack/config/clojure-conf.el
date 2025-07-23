;;; clojure-conf.el --- Clojure Config

;;; Commentary:

;;; Code:

(live-add-pack-lib "clojure-mode")

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.boot\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.bb\\'" . clojure-mode))
  :defines clojure-mode-syntax-table
  :functions put-clojure-indent

  :init
  (bind-keys :prefix-map ar-emacs-kaocha-prefix-map
             :prefix "C-c k"
             ("t" . kaocha-runner-run-test-at-point)
             ("r" . kaocha-runner-run-tests)
             ("a" . kaocha-runner-run-all-tests)
             ("w" . kaocha-runner-show-warnings)
             ("h" . kaocha-runner-hide-windows))

  :hook
  ((clojure-mode clojurescript-mode clojurec-mode) . paredit-mode)
  ((clojure-mode clojurescript-mode clojurec-mode) . subword-mode)
  ((clojure-mode clojurescript-mode clojurec-mode) . rainbow-delimiters-mode)
  ((clojure-mode clojurescript-mode clojurec-mode) . flyspell-prog-mode)
  ((clojure-mode clojurescript-mode clojurec-mode) . company-mode)
  ((clojure-mode clojurescript-mode clojurec-mode) . hl-todo-mode)

  :custom
  (clojure-toplevel-inside-comment-form t "Eval forms in (comment) as if top level")
  (clojure-verify-major-mode nil "Disable extension verification cause it causes stack overflow")
  (clojure-defun-indents '(fold pfold slice describe testing given using with it do-it go-loop defprotocol defrecord
                           deftype defroutes async eval-str deftask start stop read-eval-call-test alet mlet fact
                           eval-in capture-output alias in-parallel either fdef html5 js-await defhandler))

  :bind (:map clojure-mode-map
         ("C-c l l" . align-cljjet)
         ("M-t" . live-transpose-words-with-hyphens)
         ("C-c C-z" . nil)
         ("C-h" . help-command)
         ;; better twice because I often forget
         ("C-t C-s" . clojure-toggle-keyword-string)
         ("C-t C-k" . clojure-toggle-keyword-string))

  :config
  (require 'flycheck-clj-kondo)

  ;; Treat hyphens as a word character when transposing words
  (defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
    (let ((st (make-syntax-table clojure-mode-syntax-table)))
      (modify-syntax-entry ?- "w" st)
      st))

  (defun live-transpose-words-with-hyphens (arg)
    "Treat hyphens as a word character when transposing words"
    (interactive "*p")
    (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
     (transpose-words arg)))

    (put-clojure-indent 'pdoseq 1)

    (put-clojure-indent 'for-all 1)

    (put-clojure-indent 'GET 2)
    (put-clojure-indent 'POST 2)
    (put-clojure-indent 'PUT 2)
    (put-clojure-indent 'DELETE 2)
    (put-clojure-indent 'HEAD 2)
    (put-clojure-indent 'ANY 2)
    (put-clojure-indent 'context 2)

    (put-clojure-indent 'as-element 0)

    (put-clojure-indent 'trace-forms (lambda (_ _) 0)) ;; re-frame macros

    (put-clojure-indent 'is-resolved 1)                ;; unbroken-promises
    (put-clojure-indent '->files 1)
    (put-clojure-indent 'stub 1)                       ;; shrubbery
    (put-clojure-indent 'mock 1)                       ;; shrubbery
    (put-clojure-indent 'spy 1)                        ;; shrubbery

    (put-clojure-indent 'when-joined '(2 :form :form (0))) ;; cohesic

    (put-clojure-indent 'merr/-> :->)                  ;; merr
    (put-clojure-indent 'merr/->> :->>)                ;; merr

    (put-clojure-indent 'cond-> 0)                     ;; custom for work
    (put-clojure-indent 'cond->> 0)                    ;; custom for work
    )

;;; clojure-conf.el ends here
