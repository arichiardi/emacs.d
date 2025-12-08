;;; rcirc-conf.el -- The rcirc configuration
;;
;; Author: Andrea Richiardi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Custom configuration

(setq live-irc-logs-folder (expand-file-name "irc" live-tmp-dir))

(defun ar-emacs--rcirc-conf-hook ()
  "Setup rcirc and stuff."
  (rcirc-track-minor-mode 1)
  (flyspell-mode 1)
  ;; IRC buffers are constantly growing. If you want to see as much
  ;; as possible at all times, you would want the prompt at the
  ;; bottom of the window when possible. The following snippet uses a
  ;; local value for scroll-conservatively to achieve this:
  (set (make-local-variable 'scroll-conservatively)
       8192)
  (emojify-mode))

(use-package rcirc
  :hook ((rcirc-mode . ar-emacs--rcirc-conf-hook))
  :custom ((rcirc-server-alist '(("irc.libera.chat" :port 6697 :encryption tls
                                  :channels ("#emacs"
                                             ;; "#common-lisp.net"
                                             ;; "#lisp"
                                             ;; "#lispcafe"
                                             ;; "#org-roam"
                                             ;; "#rofi"
                                             "#LineageOS"
                                             "#searxng"))
                                 ("irc.oftc.net" :port 6697 :encryption tls
                                  :channels ("#openwrt")))
                               "Setup the server list")
           (rcirc-log-directory live-irc-logs-folder "Set the rcirc logs folder")
           (rcirc-default-nick "arichiardi" "Set the nick")
           (rcirc-default-user-name "arichiardi" "Set username")
           (rcirc-default-full-name "Andrea Richiardi" "Set full name")
           (rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY") "Suppress unimportant join/leave messages"))
  :bind ((:map rcirc-mode-map
               ("C-<up>" . rcirc-insert-prev-input)
               ("C-<down>" . rcirc-insert-next-input))))

;; custom load from ~/.authinfo.gpg
;; From Spacemacs rcirc layer
(defadvice rcirc (before rcirc-read-from-authinfo activate)
  "Initialize authinfo.
Allow rcirc to read authinfo from ~/.authinfo.gpg via the
auth-source API.  This doesn't support the chanserv auth
method. "
  (require 'auth-source)
  (if (file-exists-p "~/.authinfo.gpg")
      (dolist (p (auth-source-search :port '("nickserv" "bitlbee" "quakenet" "sasl")
                                     :require '(:port :user :secret)
                                     ;; increase if more
                                     :max 10))
        (let ((secret (plist-get p :secret))
              (method (intern (plist-get p :port))))
          (add-to-list
           'rcirc-authinfo
           (list (plist-get p :host) method (plist-get p :user)
                 (if (functionp secret) (funcall secret) secret)))))
    (message "Cannot find file ~/.authinfo.gpg")))
