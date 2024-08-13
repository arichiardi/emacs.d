;;; cosmetic.el --- Cosmetic

;;; Commentary:

;;; Code:

(setq font-lock-maximum-decoration t)

;; Line-wrapping
(set-default 'fill-column 72)

;; get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; http://stackoverflow.com/questions/7874548/emacs-23-3-1-whitespace-style
(setq whitespace-display-mappings
      '(
    (space-mark   ?\     [? ]) ;; use space not dot
    (space-mark   ?\xA0  [?\u00A4]     [?_])
    (space-mark   ?\x8A0 [?\x8A4]      [?_])
    (space-mark   ?\x920 [?\x924]      [?_])
    (space-mark   ?\xE20 [?\xE24]      [?_])
    (space-mark   ?\xF20 [?\xF24]      [?_])
    (newline-mark ?\n    [?$ ?\n])
    (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])))

(setq whitespace-style (quote (face trailing lines tabs newline space-mark tab-mark newline-mark)))

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
