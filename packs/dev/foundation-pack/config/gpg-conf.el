;;; gpg-conf.el --- GnuPG configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Routes gpg-agent passphrase prompts to the Emacs minibuffer.
;; `pinentry-dispatch` (see the ar-settings repo) reads the
;; `PINENTRY_USER_DATA` hint from the environment of the process that
;; calls gpg. The default ("emacs") is set here. Exporting the variable
;; from the shell overrides it, because `exec-path-from-shell` copies it
;; after this file is loaded.

(setenv "PINENTRY_USER_DATA" "emacs")

;;; gpg-conf.el ends here
