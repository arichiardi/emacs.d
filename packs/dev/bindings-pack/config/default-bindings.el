;;; default-bindings.el --- Default Emacs Live bindings

;;; Commentary:

;; C-c Groups
;;
;;e - eval and replace
;;b - winner undo
;;f - winner redo
;;g - git gutter
;;l - lispy shortcuts (i.e. paredit and clojure specific fns)
;;m - Emacs eval shortcuts
;;t - text shortcuts
;;i - utf8 char shortcuts
;;j - quick-jump shortcuts
;;d - diff shortcuts
;;p - project shortcuts
;;s - show popupwindows
;;w - window and buffer shortcuts
;;x - enabling modes

;;; Code:

;; Distinguish C-m from RET - https://emacs.stackexchange.com/a/20241/3979
(define-key input-decode-map [?\C-m] [C-m])

;; gracefully kill Emacs --daemon
(global-set-key (kbd "C-x C-M-c") 'save-buffers-kill-emacs)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; winner undo and redo
(global-set-key (kbd "C-c b") 'winner-undo)
(global-set-key (kbd "C-c f") 'winner-redo)

;; Git Gutter
(global-set-key (kbd "C-c g g") 'git-gutter:toggle)
(global-set-key (kbd "C-c g u") 'git-gutter:update-all-windows)
(global-set-key (kbd "C-c g r") 'git-gutter:revert-hunk)
(global-set-key (kbd "C-c g s") 'git-gutter:stage-hunk)
(global-set-key (kbd "<M-prior>") 'git-gutter:previous-hunk)
(global-set-key (kbd "<M-next>") 'git-gutter:next-hunk)
(global-set-key (kbd "C-c g d") 'git-gutter:popup-hunk)

;; Jump to next/previous diff
(global-set-key (kbd "C-c g p") 'git-gutter:previous-diff)
(global-set-key (kbd "C-c g n") 'git-gutter:next-diff)
(global-set-key (kbd "C-c g d") 'git-gutter:popup-diff)
(global-set-key (kbd "C-c g r") 'git-gutter:revert-hunk)

;; text manipulation shortcuts
(global-set-key (kbd "C-c t b") 'untabify-buffer)
(global-set-key (kbd "C-c t r") 'untabify)
(global-set-key (kbd "C-c t c") 'whitespace-cleanup)
(global-set-key (kbd "C-c t d") 'duplicate-dwim)

;; emacs-lisp shortcuts
(global-set-key (kbd "C-c m s") 'eval-and-replace) ;swap
(global-set-key (kbd "C-c m b") 'eval-buffer)
(global-set-key (kbd "C-c m e") 'eval-last-sexp)
(global-set-key (kbd "C-c m i") 'eval-expression)
(global-set-key (kbd "C-c m d") 'eval-defun)
(global-set-key (kbd "C-c m n") 'eval-print-last-sexp)
(global-set-key (kbd "C-c m r") 'eval-region)

;; funky characters
(global-set-key (kbd "C-c i l") (lambda () (interactive) (insert "λ")))
(global-set-key (kbd "C-c i n") (lambda () (interactive) (insert "ℕ")))
(global-set-key (kbd "C-c i i") (lambda () (interactive) (insert "∞")))
(global-set-key (kbd "C-c i .") (lambda () (interactive) (insert "×")))
(global-set-key (kbd "C-c i 0") (lambda () (interactive) (insert "∅")))
(global-set-key (kbd "C-c i u") (lambda () (interactive) (insert "∪")))
(global-set-key (kbd "C-c i s") (lambda () (interactive) (insert "♯")))
(global-set-key (kbd "C-c i f") (lambda () (interactive) (insert "♭")))
(global-set-key (kbd "C-c i p") (lambda () (interactive) (insert "£")))

(global-set-key (kbd "C-c j p") 'quick-jump-go-back)
(global-set-key (kbd "C-c j b") 'quick-jump-go-back)
(global-set-key (kbd "C-c j m") 'quick-jump-push-marker)
(global-set-key (kbd "C-c j n") 'quick-jump-go-forward)
(global-set-key (kbd "C-c j f") 'quick-jump-go-forward)
(global-set-key (kbd "C-c j c") 'quick-jump-clear-all-marker)

;;diff shortcuts
(global-set-key (kbd "C-c d f") 'diff-buffer-with-file)

(global-set-key (kbd "C-c s m") 'live-show-messages)
(global-set-key (kbd "C-c s p") 'helm-eshell-prompts-all)
(global-set-key (kbd "C-c s e") 'string-edit-at-point)

;;window and buffer movement
(global-set-key (kbd "C-c w s") 'swap-windows)
(global-set-key (kbd "C-c w r") 'rotate-windows)
(global-set-key (kbd "C-c w p") 'buf-move-up)
(global-set-key (kbd "C-c w n") 'buf-move-down)
(global-set-key (kbd "C-c w b") 'buf-move-left)
(global-set-key (kbd "C-c w f") 'buf-move-right)
(global-set-key (kbd "C-c w .") 'shrink-window-horizontally)
(global-set-key (kbd "C-c w ,") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c w /") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "C-c w '") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "C-c w <up>") 'buf-move-up)
(global-set-key (kbd "C-c w <down>") 'buf-move-down)
(global-set-key (kbd "C-c w <left>") 'buf-move-left)
(global-set-key (kbd "C-c w <right>") 'buf-move-right)
(global-set-key (kbd "C-c w o <prior>") 'scroll-other-window-down)
(global-set-key (kbd "C-c w o <next>") 'scroll-other-window)

(define-key paredit-mode-map (kbd "C-c l k") 'paredit-splice-sexp-killing-forward)
(define-key paredit-mode-map (kbd "C-c l w") 'paredit-splice-sexp-killing-backward)
(define-key paredit-mode-map (kbd "C-c l l") 'align-cljlet)
(define-key paredit-mode-map (kbd "C-c l t") 'fill-paragraph)
(define-key paredit-mode-map (kbd "C-c l j") 'live-paredit-forward-slurp-sexp-neatly)
(define-key paredit-mode-map (kbd "C-M-e")   'paredit-backward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-s")   'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-j")   'live-paredit-forward-slurp-sexp-neatly)
(define-key paredit-mode-map (kbd "C-M-y")   'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-z")   'align-cljlet)
(define-key paredit-mode-map (kbd "M-S")     'paredit-split-sexp)
(define-key paredit-mode-map (kbd "M-s")     'paredit-splice-sexp)
(define-key paredit-mode-map (kbd "M-j")     'paredit-join-sexps)
(define-key paredit-mode-map (kbd "M-P")     'live-paredit-previous-top-level-form)
(define-key paredit-mode-map (kbd "M-N")     'live-paredit-next-top-level-form)
(define-key paredit-mode-map (kbd "C-M-f")   'live-paredit-forward)
(define-key paredit-mode-map (kbd "M-q")     'live-paredit-reindent-defun)
(define-key paredit-mode-map (kbd "M-d")     'live-paredit-forward-kill-sexp)
(define-key paredit-mode-map (kbd "M-k")     'live-paredit-backward-kill)
(define-key paredit-mode-map (kbd "M-\\")    'live-paredit-delete-horizontal-space)
(define-key paredit-mode-map (kbd "C-M-i")   'paredit-forward-down)
(define-key paredit-mode-map (kbd "C-M-n")   'paredit-forward-up)
(define-key paredit-mode-map (kbd "C-M-p")   'paredit-backward-down)
(define-key paredit-mode-map (kbd "C-M-u")   'paredit-backward-up)
(define-key paredit-mode-map (kbd "M-T")     'transpose-sexps)
(define-key paredit-mode-map (kbd "C-M-k")   'live-paredit-copy-sexp-at-point)

;; increment and decrement number at point
(global-set-key (kbd "C-M-_")  'live-decrement-number-at-point)
(global-set-key (kbd "M-=")    'live-increment-number-at-point)
(global-set-key (kbd "C-M-=")    'live-increment-number-at-point)

;; browse kill ring (visual paste)
(global-set-key (kbd "M-y") 'browse-kill-ring)

;; make C-] and M-] cut and copy respectively
(global-set-key (kbd "C-]") 'kill-region)
(global-set-key (kbd "M-]") 'kill-ring-save)

;; mark current function
(global-set-key (kbd "C-x C-p") 'mark-defun)

;; use delete-horizontal-space to completely nuke all whitespace
(global-set-key (kbd "M-SPC ") 'live-delete-whitespace-except-one)

;; allow the deletion of words:
;; backward kill word (forward kill word is M-d)
(global-set-key (kbd "C-\\") 'backward-kill-word)
(define-key paredit-mode-map (kbd "C-\\") 'paredit-backward-kill-word)

;; kill line backwards
(global-set-key (kbd "M-k") 'live-backwards-kill-line)

;; kill regions
(global-set-key (kbd "C-x C-k") 'kill-region)

;; set the mark
(global-set-key (kbd "C-SPC") 'set-mark-command)

;; repeat previous command
(global-set-key (kbd "M-'") 'repeat)

;; scroll other window
(global-set-key (kbd "C-M-]") 'scroll-other-window)
(global-set-key (kbd "C-M-[") 'scroll-other-window-down)

;; fast vertical naviation
(global-set-key  (kbd "M-U") (lambda () (interactive) (forward-line -10)))
(global-set-key  (kbd "M-D") (lambda () (interactive) (forward-line 10)))
(global-set-key  (kbd "M-p") 'outline-previous-visible-heading)
(global-set-key  (kbd "M-n") 'outline-next-visible-heading)

;; Align your code in a pretty way
(global-set-key (kbd "C-x \\") 'align-regexp)

;; comment region
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

(global-set-key (kbd "C-s")   'isearch-forward)
(global-set-key (kbd "C-r")   'isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)

;; File
(global-set-key (kbd "M-`")       'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b")   'ibuffer)

;; Window switching
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Helm
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-x f")                        'helm-recentf)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key [remap bookmark-jump]                'helm-filtered-bookmarks)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "S-<f2>")                       'helm-execute-kmacro)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-mini)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(global-set-key (kbd "M-s")                          'helm-do-grep-ag)
(global-set-key (kbd "C-x r p")                      'helm-projects-history)
(global-set-key (kbd "C-x r c")                      'helm-addressbook-bookmarks)
(global-set-key (kbd "C-x r d")                      'helm-dictionary)
(global-set-key (kbd "C-x C-b")                      'helm-buffers-list)
;; What to do with these?
;; (global-set-key (kbd "C-h i")                        'helm-info)
;; (global-set-key (kbd "C-h a")                        'helm-apropos)
;; (global-set-key (kbd "C-h C-d")                      'helm-debug-open-last-log)
;; (global-set-key (kbd "C-h C-l")                      'helm-locate-library)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Ace jump mode
(global-set-key (kbd "C-o") 'ace-jump-mode)

;; Show documentation/information with M-RET
(define-key lisp-mode-shared-map (kbd "M-RET") 'live-lisp-describe-thing-at-point)

(global-set-key (kbd "C-x o") 'win-switch-dispatch)

(global-set-key (kbd "C-x !") 'live-server-kill-terminal)

;;; Multiple Cursors
(global-set-key (kbd "<C-S-mouse-1>") 'mc/add-cursor-on-click) ; works just in a X window
(global-set-key (kbd "<C-next>") 'mc/mark-next-like-this)
(global-set-key (kbd "<C-prior>") 'mc/mark-previous-like-this)

;; phi-search
(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)
(global-set-key (kbd "M-%") 'phi-search-query)

;; C-c x custom group for enabling modes
(define-key prog-mode-map (kbd "C-c x s") 'subword-mode)
(define-key prog-mode-map (kbd "C-c x w") 'whitespace-mode)
(define-key prog-mode-map (kbd "C-c x l") 'display-line-numbers-mode)

;; custom zoom bindings
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; projectile
(global-set-key (kbd "s-d") 'helm-projectile-find-dir)
(global-set-key (kbd "s-p") 'projectile-persp-switch-project)
(global-set-key (kbd "s-f") 'helm-projectile-find-file)
(global-set-key (kbd "s-b") 'helm-projectile-switch-to-buffer)
(global-set-key (kbd "s-.") 'projectile-find-tag)
(global-set-key (kbd "s-s") 'helm-projectile-ag)
(global-set-key (kbd "s-g") 'helm-projectile-grep)
(global-set-key (kbd "s-M-t") 'projectile-toggle-between-implementation-and-test)

;; company-mode
(global-set-key (kbd "<backtab>") 'company-complete)

;;; default-bindings.el ends here
