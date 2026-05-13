;;; early-init.el --- Emacs early initialization -*- lexical-binding:t; no-byte-compile:t -*-

;;; Commentary:

;; This is take from doom-emacs.
;;
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
;; (advice-add #'x-apply-session-resources :override #'ignore)

;; Set eln-cache dir: https://emacs.stackexchange.com/a/70478
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "~/.cache/eln-cache" user-emacs-directory)))

;; https://github.com/d12frosted/homebrew-emacs-plus?tab=readme-ov-file#emacs-29-1
(add-to-list 'default-frame-alist '(undecorated . t))

;; https://docs.emacsmirror.org/borg/Bootstrapping-from-scratch.html
(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

(setq package-enable-at-startup nil)

;; Golden-ratio frame sizing on Wayland sessions only.
(defconst live-golden-ratio
  (/ (+ 1 (sqrt 5.0)) 2.0)
  "The golden ratio φ.")

(defconst live-golden-frame-scale
  0.5
  "Fraction of the monitor to fill with the golden‑ratio frame.")

(defun live-golden-frame-size (&optional frame)
  "Return a cons (WIDTH . HEIGHT) that obeys the golden‑ratio.
Computes dimensions in pixel space first, then converts to cells,
so the visual aspect ratio is correct even though chars are
rectangular.

Uses FRAME if supplied, otherwise the selected frame."
  (let* ((fm        (condition-case nil
                        (frame-monitor-attributes frame)
                      (error nil)))
         (geom      (and fm (alist-get 'geometry fm)))
         (disp-w    (or (and geom (nth 2 geom))
                        (and frame (frame-pixel-width frame))
                        (display-pixel-width)))
         (disp-h    (or (and geom (nth 3 geom))
                        (and frame (frame-pixel-height frame))
                        (display-pixel-height)))
         (char-w    (frame-char-width frame))
         (char-h    (frame-char-height frame))
         ;; scaled available area
         (avail-w   (floor (* disp-w live-golden-frame-scale)))
         (avail-h   (floor (* disp-h live-golden-frame-scale)))
         ;; golden-ratio candidates in pixels
         (cand1-w   avail-w)
         (cand1-h   (floor (/ cand1-w live-golden-ratio)))
         (cand2-h   avail-h)
         (cand2-w   (floor (* cand2-h live-golden-ratio)))
         ;; pick the one that fits
         (px-w      (if (<= cand1-h avail-h) cand1-w cand2-w))
         (px-h      (if (<= cand1-h avail-h) cand1-h cand2-h))
         ;; convert to cells
         (cells-w   (/ px-w char-w))
         (cells-h   (/ px-h char-h)))
    (cons cells-w cells-h)))

(defun live-resize-to-golden (frame)
  "Resize FRAME to golden‑ratio dimensions after it is created."
  (let* ((size      (live-golden-frame-size frame))
         (w         (car size))
         (h         (cdr size)))
    (message "live-resize-to-golden: %d×%d cells" w h)
    (sleep-for 0.1)
    (set-frame-size frame w h)))

(when (string-equal "wayland" (getenv "XDG_SESSION_TYPE"))
  (add-hook 'after-make-frame-functions #'live-resize-to-golden))

;;; early-init.el ends here
