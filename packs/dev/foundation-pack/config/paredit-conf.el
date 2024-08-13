;;; paredit-conf.el --- Paredit config
;;; Commentary:
;;
;; Configure paredit

;;; Code:

(require 'paredit)
(require 'thingatpt)

(defun live-paredit-next-top-level-form ()
  (interactive)
  (while (ignore-errors (paredit-backward-up) t))
  (live-paredit-forward))

(defun live-paredit-previous-top-level-form ()
  (interactive)
  (if (ignore-errors (paredit-backward-up) t)
      (while (ignore-errors (paredit-backward-up) t))
    (paredit-backward)))

(defun live-paredit-forward (&optional arg)
  "Feels more natural to move to the beginning of the next item
   in the sexp, not the end of the current one."
  (interactive "P")
  (let ((n (or arg 1)))
    (if (and (not (paredit-in-string-p))
             (save-excursion
               (ignore-errors
                 (forward-sexp)
                 (forward-sexp)
                 t)))
        (dotimes (i n) (progn (forward-sexp)
                              (forward-sexp)
                              (backward-sexp)))

      (paredit-forward arg))))

(defun live-paredit-forward-slurp-sexp-neatly (&optional arg)
  (interactive "P")
  (save-excursion
    (cond ((or (paredit-in-comment-p)
               (paredit-in-char-p))
           (error "Invalid context for slurping S-expressions."))
          ((paredit-in-string-p)
           (paredit-forward-slurp-into-string arg))
          (t

           (save-excursion
             (paredit-forward-up)
             (paredit-backward-down)
             (paredit-forward-slurp-sexp arg)
             (just-one-space)))))
  (when (not (save-excursion
               (ignore-errors
                 (backward-sexp)
                 t)))
    (delete-horizontal-space)))



(defun live-paredit-forward-kill-sexp (&optional arg)
  (interactive "p")
  (cond ((or (paredit-in-comment-p)
             (paredit-in-string-p)) (kill-word (or arg 1)))
        (t (kill-sexp (or arg 1)))))

(defun live-paredit-backward-kill-sexp (&optional arg)
  (interactive "p")
  (cond ((or (paredit-in-comment-p)
             (paredit-in-string-p)) (backward-kill-word (or arg 1)))
        (t (backward-kill-sexp (or arg 1)))))

(defun live-paredit-backward-kill ()
  (interactive)
  (let ((m (point-marker)))
    (paredit-backward-up)
    (forward-char)
    (delete-region (point) m)))

(defun live-paredit-delete-horizontal-space ()
  (interactive)
  (just-one-space -1)
  (paredit-backward-delete))

(defun live-paredit-tidy-trailing-parens ()
  (interactive)
  (save-excursion
    (while (ignore-errors (paredit-forward-up) t))
    (backward-char)
    (live-paredit-delete-horizontal-space)
    (while
        (or
         (eq (char-before) ?\))
         (eq (char-before) ?\})
         (eq (char-before) ?\]))
      (backward-char)
      (live-paredit-delete-horizontal-space))))

(defun live-paredit-reindent-defun (&optional arg)
  "Reindent the definition that the point is on. If the point is
  in a string or a comment, fill the paragraph instead, and with
  a prefix argument, justify as well. Doesn't mess about with
  Clojure fn arglists when filling-paragraph in docstrings.

  Also tidies up trailing parens when in a lisp form"
  (interactive "P")
  (cond ((paredit-in-comment-p) (fill-paragraph arg))
        ((paredit-in-string-p) (progn
                                 (save-excursion
                                   (paredit-forward-up)
                                   (insert "\n"))
                                 (fill-paragraph arg)
                                 (save-excursion
                                   (paredit-forward-up)
                                   (delete-char 1))))
        (t (when (not (live-paredit-top-level-p))
             (progn (save-excursion
                      (end-of-defun)
                      (beginning-of-defun)
                      (indent-sexp))
                    (live-paredit-tidy-trailing-parens))))))


(defun live-paredit-forward-down (&optional arg)
  "Doesn't freeze Emacs if attempted to be called at end of
   buffer. Otherwise similar to paredit-forward-down."
  (interactive "P")
  (if (save-excursion
        (forward-comment (buffer-size))
        (not (live-end-of-buffer-p)))
      (paredit-forward-down arg)
    (error "unexpected end of buffer")))

(defun live-paredit-top-level-p ()
  "Returns true if point is not within a given form i.e. it's in
  toplevel 'whitespace'"
  (not
   (save-excursion
     (ignore-errors
       (paredit-forward-up)
       t))))

(defun live-paredit-copy-sexp-at-point ()
  (interactive)
    (kill-new (thing-at-point 'sexp)))

(defun paredit--is-at-start-of-sexp ()
  (and (looking-at "(\\|\\[")
       (not (nth 3 (syntax-ppss))) ;; inside string
       (not (nth 4 (syntax-ppss))))) ;; inside comment

(defun paredit-duplicate-closest-sexp ()
  (interactive)
  ;; skips to start of current sexp
  (while (not (paredit--is-at-start-of-sexp))
    (paredit-backward))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

;; http://whattheemacsd.com/setup-paredit.el-03.html
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)

(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(with-eval-after-load "paredit"
  (define-key paredit-mode-map (kbd "C-h") 'help-command)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
  ;; The following was overridding the below
  ;; (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-wrap-round-from-behind)
  (define-key paredit-mode-map (kbd "C-<left>") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-<right>") 'live-paredit-forward-slurp-sexp-neatly)
  (define-key paredit-mode-map (kbd "C-M-<left>") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-M-<right>") 'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-<up>") 'live-paredit-previous-top-level-form)
  (define-key paredit-mode-map (kbd "M-<down>") 'live-paredit-next-top-level-form)
  (define-key paredit-mode-map (kbd "M-SPC ") 'live-paredit-tidy-trailing-parens)
  (define-key paredit-mode-map (kbd "C-M-/") 'clju-toggle-ignore-form)
  (define-key paredit-mode-map (kbd "C-M-f") 'paredit-forward)
  (define-key paredit-mode-map (kbd "C-M-b") 'paredit-backward)
  (define-key paredit-mode-map (kbd "M-<") 'ar-emacs-paredit-first-buffer-form))

;; End:
;;; paredit-conf.el ends here
