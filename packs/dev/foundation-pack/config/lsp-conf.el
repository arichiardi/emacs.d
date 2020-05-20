;;; lsp-conf.el --- LSP Config

;;; Commentary:

;;; Code:

(setq lsp-keymap-prefix "C-l")

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-enable-snippet t "Enable snippet support"))

(use-package helm-lsp
  :commands (helm-lsp-workspace-symbol helm-lsp-code-actions))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (add-hook 'lsp-mode-hook #'lsp-ui-mode))

;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
(defun ar-emacs--add-yas-backend (backend)
  "Add yasnippet to the BACKEND."
  (if (or (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(use-package company-lsp
  :defer t
  :after company

  :config
  (setq company-backends (mapcar #'ar-emacs--add-yas-backend company-backends))

  :custom
  ((company-lsp-enable-snippet t "Enable snippet support")
   (company-lsp-cache-candidates t "Enable cache candidates")))

;;; lsp-conf.el ends here
