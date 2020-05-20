;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Clojure LIVE
;;
;; This is where everything starts.  Do you remember this place?
;; It remembers you...

;;; Code:

(defvar live-ascii-art-logo ";;
;;                       @@@@@@@@@@@@@@@
;;                  @@@@@@@@@@((/((/@@@@@@@@@@
;;               @@@@@@////////////////////(@@@@@
;;            @@@@@////////////////////////////(@@@@
;;          @@@@//////////////////////////////////#@@@
;;        @@@@//////////////////////////////////////(@@@
;;       @@@//@@@@@@@@@@(////@@@@@@@@/////////////////%@@
;;      @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@//////////////@@@
;;     @@@@@@@@@#(((((@@@@@@@%#########&@@@@@////////////@@@
;;    @@@@@@((((((((@@@@%%@@@@@###########%@@@@///////////@@@
;;   @@@@%(((((((((@@@%%%%%%@@@@############%@@@///////////@@@
;;  @@@@((((((((((@@@%%%%%%%%@@@@#############@@@//////////@@@@
;;  @@@((((((((((@@@%%%%%%%%%%@@@##############@@@//////////@@@
;;  @@%(((((((((&@@%%%%%%%%%%%%@@@#############@@@//////////@@@
;;  @@((((((((((@@@%%%%%%%%%%%%@@@%############%@@&/////////&@@
;;  @@((((((((((@@@%%%%%%%%%%%@@&@@############%@@&/////////&@@
;;  @@%(((((((((&@@%%%%%%%%%%@@%%@@@###########@@@//////////@@@
;;  @@@((((((((((@@@%%%%%%%%@@%%%&@@#########@@@@@/////////(@@@
;;  @@@(((((((((((@@@%%%%%%@@&%%%%@@@@########@@@/////////(@@@@
;;   @@@(((((((((((@@@%%%%@@@%%%%%%@@@@#####@@@@/////////@@@@@
;;    @@@(((((((((((@@@@%@@@%%%%%%%%@@@@@#@@@@#////////@@@@@@
;;     @@@((((((((((((@@@@@@%%%%%%%%%@@@@@@@@@@@@@@@@@@@@@@@
;;      @@@((((((((((((((@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;       @@@(((((((((((((((((@@@@@@@@@@((((((((((((((@@@@
;;        @@@@((((((((((((((((((((((((((((((((((((((&@@@
;;          @@@@((((((((((((((((((((((((((((((((((@@@@@
;;            @@@@@((((((((((((((((((((((((((((&@@@@
;;               @@@@@@((((((((((((((((((((@@@@@@@
;;                   @@@@@@@@@@@@@@@@@@@@@@@@@")

(princ (concat "\n\n" live-ascii-art-logo "\n\n"))

(add-to-list 'command-switch-alist
             (cons "--live-safe-mode"
                   (lambda (_)
                     (message "Running in safe mode..."))))

(defvar live-safe-modep
      (if (member "--live-safe-mode" command-line-args)
          "debug-mode-on"
        nil))

(defvar initial-scratch-message "
;; I'm sorry, Emacs Live failed to start correctly.
;; Hopefully the issue will be simple to resolve.
;;
;; First up, could you try running Emacs Live in safe mode:
;;
;;    emacs --live-safe-mode
;;
;; This will only load the default packs. If the error no longer occurs
;; then the problem is probably in a pack that you are loading yourself.
;; If the problem still exists, it may be a bug in Emacs Live itself.
;;
;; In either case, you should try starting Emacs in debug mode to get
;; more information regarding the error:
;;
;;    emacs --debug-init
;;
;; Please feel free to raise an issue on the Gihub tracker:
;;
;;    https://github.com/overtone/emacs-live/issues
;;
;; Alternatively, let us know in the mailing list:
;;
;;    http://groups.google.com/group/emacs-live
;;
;; Good luck, and thanks for using Emacs Live!
;;
;;                _.-^^---....,,--
;;            _--                  --_
;;           <          SONIC         >)
;;           |       BOOOOOOOOM!       |
;;            \._                   _./
;;               ```--. . , ; .--'''
;;                     | |   |
;;                  .-=||  | |=-.
;;                  `-=#$%&%$#=-'
;;                     | ;  :|
;;            _____.,-#%&$@%#&#~,._____
;;      May these instructions help you raise
;;                  Emacs Live
;;                from the ashes
")

(defvar live-supported-emacsp t)

(when (version< emacs-version "24.3")
  (setq live-supported-emacsp nil)
  (setq initial-scratch-message (concat "
;;                _.-^^---....,,--
;;            _--                  --_
;;           <          SONIC         >)
;;           |       BOOOOOOOOM!       |
;;            \._                   _./
;;               ```--. . , ; .--'''
;;                     | |   |
;;                  .-=||  | |=-.
;;                  `-=#$%&%$#=-'
;;                     | ;  :|
;;            _____.,-#%&$@%#&#~,._____
;;
;; I'm sorry, Emacs Live is only supported on Emacs 24.3+.
;;
;; You are running: " emacs-version "
;;
;; Please upgrade your Emacs for full compatibility.
;;
;; Latest versions of Emacs can be found here:
;;
;; OS X GUI     - http://emacsformacosx.com/
;; OS X Console - via homebrew (http://mxcl.github.com/homebrew/)
;;                brew install emacs
;; Windows      - http://alpha.gnu.org/gnu/emacs/windows/
;; Linux        - Consult your package manager or compile from source

"))
  (let* ((old-file (concat (file-name-as-directory "~") ".emacs-old.el")))
    (if (file-exists-p old-file)
      (load-file old-file)
      (error (concat "Oops - your emacs isn't supported. Emacs Live only works on Emacs 24.3+ and you're running version: " emacs-version ". Please upgrade your Emacs and try again, or define ~/.emacs-old.el for a fallback")))))

(let ((emacs-live-directory (getenv "EMACS_LIVE_DIR")))
  (when emacs-live-directory
    (setq user-emacs-directory emacs-live-directory)))

(when live-supported-emacsp
  ;; Store live base dirs, but respect user's choice of `live-root-dir'
  ;; when provided.
  (setq live-root-dir (if (boundp 'live-root-dir)
                          (file-name-as-directory live-root-dir)
                        (if (file-exists-p (expand-file-name "manifest.el" user-emacs-directory))
                            user-emacs-directory)
                        (file-name-directory (or
                                              load-file-name
                                              buffer-file-name))))

(setq
 live-tmp-dir      (file-name-as-directory (concat live-root-dir "tmp"))
 live-etc-dir      (file-name-as-directory (concat live-root-dir "etc"))
 live-pscratch-dir (file-name-as-directory (concat live-tmp-dir  "pscratch"))
 live-lib-dir      (file-name-as-directory (concat live-root-dir "lib"))
 live-packs-dir    (file-name-as-directory (concat live-root-dir "packs"))
 live-autosaves-dir(file-name-as-directory (concat live-tmp-dir  "autosaves"))
 live-backups-dir  (file-name-as-directory (concat live-tmp-dir  "backups"))
 live-custom-dir   (file-name-as-directory (concat live-etc-dir  "custom"))
 live-load-pack-dir nil
 live-disable-zone nil)

;; create tmp dirs if necessary
(make-directory live-etc-dir t)
(make-directory live-tmp-dir t)
(make-directory live-autosaves-dir t)
(make-directory live-backups-dir t)
(make-directory live-custom-dir t)
(make-directory live-pscratch-dir t)

;; Borg early birds
(progn
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")

  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))

  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  ;; (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  ;; (setq inhibit-startup-echo-area-message "locutus")
  ;; (setq initial-buffer-choice t)
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(progn
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(progn
  (require 'use-package)
  (setq use-package-verbose t))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name (concat live-etc-dir "/epkgs/") user-emacs-directory)))

(progn
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;; Emacs live

(load-file (concat live-root-dir "manifest.el"))

(load-file (concat live-lib-dir "live-core.el"))

(let* ((pack-names '("foundation-pack"
                     "colour-pack"
                     "lang-pack"
                     "power-pack"
                     "git-pack"
                     "clojure-pack"
                     "bindings-pack"))
       (live-dir (file-name-as-directory "stable"))
       (dev-dir  (file-name-as-directory "dev")))

  (setq live-packs (mapcar (lambda (p) (concat live-dir p)) pack-names))
  (setq live-dev-pack-list (mapcar (lambda (p) (concat dev-dir p)) pack-names)))

(defun live-version ()
  (interactive)
  (if (called-interactively-p 'interactive)
      (message "%s" (concat "This is Emacs Live " live-version))
    live-version))

;; Load `~/.emacs-live.el`. This allows you to override variables such
;; as live-packs (allowing you to specify pack loading order)
;; Does not load if running in safe mode
(let* ((pack-file (concat (file-name-as-directory "~") ".emacs-live.el")))
  (if (and (file-exists-p pack-file) (not live-safe-modep))
      (load-file pack-file)))

;; Load all packs - Power Extreme!
(mapc (lambda (pack-dir)
          (live-load-pack pack-dir))
        (live-pack-dirs))

(setq live-welcome-messages
      (if (live-user-first-name-p)
          (list (concat "Hello " (live-user-first-name) ", somewhere in the world the sun is shining for you right now.")
                (concat "Hello " (live-user-first-name) ", it's lovely to see you again. I do hope that you're well.")
                (concat (live-user-first-name) ", turn your head towards the sun and the shadows will fall behind you.")
                )
        (list  "Hello, somewhere in the world the sun is shining for you right now."
               "Hello, it's lovely to see you again. I do hope that you're well."
               "Turn your head towards the sun and the shadows will fall behind you.")))


(defun live-welcome-message ()
  (nth (random (length live-welcome-messages)) live-welcome-messages))

(when live-supported-emacsp
  (setq initial-scratch-message (concat live-ascii-art-logo " Version " live-version
                                                                (if live-safe-modep
                                                                    "
;;                                                     --*SAFE MODE*--"
                                                                  "
;;"
                                                                  ) "
;; "                                                      (live-welcome-message) "

")))
)

(if (not live-disable-zone)
    (add-hook 'emacs-startup-hook 'zone))

(if (not custom-file)
    (setq custom-file (concat live-custom-dir "custom-configuration.el")))
(when (file-exists-p custom-file)
  (load custom-file))

(message "\n\n Pack loading completed. Your Emacs is Live...\n\n")
(put 'downcase-region 'disabled nil)
