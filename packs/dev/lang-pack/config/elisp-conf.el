;;; elisp-conf.el --- Elisp Config

;;; Commentary:

;;; Code:

(defun ar-emacs-emacs-lisp-mode-hook ()
  "Custom `emacs-lisp-mode' hook."
  (company-mode)
  (paredit-mode)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'ar-emacs-emacs-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook #'ar-emacs-emacs-lisp-mode-hook)
(add-hook 'ielm-mode-hook #'ar-emacs-emacs-lisp-mode-hook)

(defun live-lisp-top-level-p ()
  "Returns true if point is not within a given form i.e. it's in
  toplevel 'whitespace'. Only works for lisp modes."
  (= 0 (car (syntax-ppss))))

(defun live-check-lisp-top-level-p ()
  "Returns true if point is not within a given form i.e. it's in
  toplevel 'whitespace'. Only works for lisp modes."
  (interactive)
  (if (live-lisp-top-level-p)
      (message "top level")
    (message "not top level")))

(defun live-whitespace-at-point-p ()
  "Returns true if the char at point is whitespace"
  (string-match "[ \n\t]" (buffer-substring (point) (+ 1 (point)))))

;;; elisp-conf.el ends here
