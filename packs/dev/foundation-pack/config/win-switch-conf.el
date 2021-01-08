;;; win-switch-conf.el --- Win-switch config
;;; Commentary:
;;
;; Configure the magit package

;;; Code:

(use-package win-switch
  :commands win-switch-dispatch

  :custom
  (win-switch-window-threshold 0)
  (win-switch-idle-time 1)

  :config
  ;; this is not great cause when you change theme it will break.
  (setq win-switch-feedback-background-color (cdr (assoc 'bg-special-cold modus-operandi-theme-default-colors-alist)))
  (setq win-switch-feedback-foreground-color (cdr (assoc 'fg-main modus-operandi-theme-default-colors-alist)))

  ;; disable majority of shortcuts
  (win-switch-set-wrap-around)
  (win-switch-set-keys '() 'up)
  (win-switch-set-keys '() 'down)
  (win-switch-set-keys '() 'left)
  (win-switch-set-keys '() 'right)
  (win-switch-set-keys '("o") 'next-window)
  (win-switch-set-keys '("p") 'previous-window)
  (win-switch-set-keys '() 'enlarge-vertically)
  (win-switch-set-keys '() 'shrink-vertically)
  (win-switch-set-keys '() 'shrink-horizontally)
  (win-switch-set-keys '() 'enlarge-horizontally)
  (win-switch-set-keys '(" " "," "m") 'other-frame)
  (win-switch-set-keys '("C-g") 'exit)
  (win-switch-set-keys '() 'split-horizontally)
  (win-switch-set-keys '() 'split-vertically)
  (win-switch-set-keys '() 'delete-window)
  (win-switch-set-keys '("\M-\C-g") 'emergency-exit))

;; End:
;;; win-switch-conf.el ends here
