;;; mouse-conf.el --- Completion Config

;;; Commentary:

;;; Code:

;; Mouse in terminal
(require 'mouse)

(defun initialise-mouse-mode (&optional frame)
  "Initialise mouse mode for the current terminal."
  (if (not frame) ;; The initial call.
      (xterm-mouse-mode 1)
    ;; Otherwise called via after-make-frame-functions.
    (if xterm-mouse-mode
        ;; Re-initialise the mode in case of a new terminal.
        (xterm-mouse-mode 1))))

;; Evaluate both now (for non-daemon emacs) and upon frame creation
;; (for new terminals via emacsclient).

;; mouse mode must be initialised for each new terminal
;; see http://stackoverflow.com/a/6798279/27782
;; (initialise-mouse-mode)

(add-hook 'after-make-frame-functions 'initialise-mouse-mode)

(setq mouse-yank-at-point t
      ;; https://stackoverflow.com/questions/1128927/how-to-scroll-line-by-line-in-gnu-emacs
      scroll-step           1
      scroll-conservatively 10000)

;;; mouse-conf.el ends here
