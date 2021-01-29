;;; zeal-at-point-conf.el --- zeal-at-point Config

;;; Commentary:

;;; Code:

(use-package zeal-at-point
  :functions zeal-at-point
  :defines (zeal-at-point-docset zeal-at-point-mode-alist)
  :bind (("C-c d z" . zeal-at-point))

  :config
  (push '(clojure-mode . "java") zeal-at-point-mode-alist)
  (push '(clojurescript-mode . ("cljs" "svg" "nodejs")) zeal-at-point-mode-alist)
  (push '(terraform-mode . "terraform") zeal-at-point-mode-alist)

  (defun ar-emacs--zeal-set-docset-from-file-extension ()
    "Set the Zeal docset from the file extension."
    (let ((file-name (buffer-file-name)))
      (pcase (file-name-extension file-name)
        ((pred (string= "rkt"))
         (setq zeal-at-point-docset "racket")))))

  (defun ar-emacs--zeal-set-docset-from-sql-product ()
    "Set the docset from the sql product."
    (pcase sql-product
      ((pred (string= "postgres"))
       (setq-local zeal-at-point-docset "psql"))))

  :hook
  (geiser-mode . ar-emacs--zeal-set-docset-from-file-extension)
  (sql-mode . ar-emacs--zeal-set-docset-from-sql-product)
  (sql-interactive-mode . ar-emacs--zeal-set-docset-from-sql-product))

;;; zeal-at-point-conf.el ends here
