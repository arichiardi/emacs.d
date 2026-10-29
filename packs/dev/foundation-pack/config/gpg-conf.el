;;; gpg-conf.el --- GnuPG configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; `PINENTRY_USER_DATA' is deliberately not set here: it routes prompts
;; to pinentry-emacs, which requires a pinentry.el server.

(setopt epg-pinentry-mode 'loopback)

;;; gpg-conf.el ends here
