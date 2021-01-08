;;; clojure-conf.el --- Clojure Config

;;; Commentary:

;;; Code:

(live-add-pack-lib "clojure-mode")

(defun live-warn-when-cider-not-connected ()
  "Print warning if the REPL has not been launched."
  (interactive)
  (message "nREPL server not connected. Run M-x cider or M-x cider-jack-in to connect."))

(defun ar-emacs--clojure-mode-hook ()
  "The clojure-mode hook lambda."
  (enable-paredit-mode)
  (rainbow-delimiters-mode)
  (flyspell-prog-mode)
  (subword-mode)
  (company-mode-on))

(use-package clojure-mode
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'" "\\.clje\\'" "\\.joke\\'" "\\.bb\\'")
  :defines clojure-mode-syntax-table

  :hook
  (clojure-mode . #'ar-emacs--clojure-mode-hook)
  ;; (clojure-mode . (lambda () (setq buffer-save-without-query t)))

  :custom
  (clojure-toplevel-inside-comment-form t "Allow coloring of (comment) form at the top level")
  (clojure-verify-major-mode nil "Disable extension verification cause it causes stack overflow")

  :bind (:map clojure-mode-map
         ("C-M-x" . live-warn-when-cider-not-connected)
         ("C-x C-e" . live-warn-when-cider-not-connected)
         ("C-c C-e" . live-warn-when-cider-not-connected)
         ("C-c C-l" . live-warn-when-cider-not-connected)
         ("C-c l l" . align-cljjet)
         ("M-t" . live-transpose-words-with-hyphens))

  :config
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

  (define-clojure-indent
    (fold 'defun)
    (pfold 'defun)
    (pdoseq 1)
    (slice 'defun)
    (for-all 1)
    (describe 'defun)
    (testing 'defun)
    (given 'defun)
    (using 'defun)
    (with 'defun)
    (it 'defun)
    (do-it 'defun)
    (go-loop 'defun)
    (defprotocol 'defun)
    (defrecord 'defun)
    (deftype 'defun)
    (deftype 'defun)
    (defroutes 'defun)
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    (async 'defun)
    (eval-str 'defun)
    (deftask 'defun)
    (start 'defun)
    (stop 'defun)
    (read-eval-call-test 'defun)
    (alet 'defun)
    (mlet 'defun)
    (fact 'defun)                  ;; midje
    (eval-in 'defun)               ;; classlojure
    (trace-forms (lambda (_ _) 0)) ;; re-frame macros
    (capture-output 'defun)        ;; lambdacd
    (alias 'defun)                 ;; lambdacd
    (in-parallel 'defun)           ;; lambdacd
    (either 'defun)                ;; lambdacd
    (fdef 'defun)
    (html5 'defun)                 ;; hiccup
    (is-resolved 1)                ;; unbroken-promises
    (is-rejected 1)                ;; unbroken-promises
    (->files 1))

  )

;;; clojure-conf.el ends here
