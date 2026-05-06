;;; cosmetic.el --- Cosmetic  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq font-lock-maximum-decoration t)

;; Line-wrapping
(set-default 'fill-column 72)

(add-hook 'prog-mode-hook
          (lambda () (setq-local fill-column 100)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default truncate-lines t)
(setq electric-indent-mode t)

;; get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; remove bells
(setq ring-bell-function 'ignore)

(defun live-set-frame-font (font-string)
  "Sets the default font and sets all frames to the same font trying to maintain window resolution. Only changes font if window system is not a basic terminal."
  (interactive "New emacs live default font: ")
  (setq default-frame-alist
        (remove-if (lambda (x)
                     (eq 'font (car x)))
                   default-frame-alist))
  (progn
   (add-to-list 'default-frame-alist (cons 'font font-string))
   (set-frame-font font-string t t)))

(defun live-set-frame-darwin-font (font-string)
  "Sets the default font and sets all frames to the same font trying to maintain window resolution. Only changes font if system-type is darwin in a window system."
  (interactive "New darwin default font: ")
  (cond
   ((eq system-type 'darwin)
    (live-set-frame-font font-string))))

;;; cosmetic.el ends here
