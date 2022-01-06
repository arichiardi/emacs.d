;;; cosmetic.el --- Cosmetic

;;; Commentary:

;;; Code:

;; FIXME: [emacs 29] It seems emacsclient requires to set backend and font with elisp alongside than
;; .Xresources. However, we had to revert because of a segmentation fault.
;; (add-to-list 'default-frame-alist '(font-backend . ftcrhb)
;;
;; NOTE: all the elisp settings need to match what is in .Xresources
(set-face-attribute 'default nil :font "JetBrains Mono-11")

(setq font-lock-maximum-decoration t)

;; Line-wrapping
(set-default 'fill-column 72)

;; get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; remove bells
(setq ring-bell-function 'ignore)

(defun live-set-frame-font (font-string)
  "Sets the default font and sets all frames to the same font trying to maintain window resolution. Only changes font if window system is not a basic terminal."
  (interactive "MNew emacs live default font: ")
  (setq default-frame-alist
        (remove-if (lambda (x)
                     (eq 'font (car x)))
                   default-frame-alist))
  (cond
   ((member (window-system) '(x w32 ns))
    (add-to-list 'default-frame-alist (cons 'font font-string))
    (set-frame-font font-string t t))))

(defun live-set-frame-darwin-font (font-string)
  "Sets the default font and sets all frames to the same font trying to maintain window resolution. Only changes font if system-type is darwin in a window system."
  (interactive "MNew darwin default font: ")
  (cond
   ((eq system-type 'darwin)
    (live-set-frame-font font-string))))

(live-set-frame-darwin-font "Menlo-12")

;;; cosmetic.el ends here
