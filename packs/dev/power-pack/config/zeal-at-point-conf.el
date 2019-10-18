(defun ar-emacs--zeal-scheme-set-docset
    ()
  "Set the docset for the scheme family."
  (let ((file-name (buffer-file-name)))
    (pcase (file-name-extension file-name)
      ((pred (string= "rkt"))
       (setq zeal-at-point-docset "racket")))))

(use-package zeal-at-point
  :bind (("C-c d z" . zeal-at-point))
  :init
  (add-hook 'geiser-mode-hook #'ar-emacs--zeal-scheme-set-docset)

  :defines zeal-at-point-mode-alist
  :custom
  (push '(clojure-mode . "java") zeal-at-point-mode-alist)
  (push '(clojurescript-mode . ("cljs" "svg" "nodejs")) zeal-at-point-mode-alist)
  (push '(terraform-mode . "terraform") zeal-at-point-mode-alist))
