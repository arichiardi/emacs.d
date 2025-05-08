;;; sql-conf.el --- SQL Config

;;; Commentary:

;;; Code:

(use-package sqlup-mode
  :custom (sqlup-blacklist (list "id")))

(use-package sql-mode
  :config
  (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
  (sql-set-product-feature 'postgres :prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] ")

  :hook
  ((sql-mode . sqlind-minor-mode)
   ;; Disabling for now (always available via commands)
   ;; (sql-mode . sqlup-mode)
   ;; (sql-interactive-mode . sqlup-mode)
   )

  :bind (:map sql-mode-map
         ("C-c C-a" . sql-product-interactive)
         ("C-c C-u r" . sqlup-capitalize-keywords-in-region)
         ("C-c C-u b" . sqlup-capitalize-keywords-in-buffer)
         :map sql-interactive-mode-map
         ("C-c C-u r" . sqlup-capitalize-keywords-in-region)
         ("C-c C-u b" . sqlup-capitalize-keywords-in-buffer))

  :custom
  (sql-password-wallet "~/.authinfo.gpg")
  ;; Note that without 'password Emacs will never check the secret wallet
  ;; (sql-postgres-login-params '(password))
  (sql-postgres-options '("-P" "pager=off" "-P" "null=(null)"))
  (sql-postgres-login-params nil)
  (sql-electric-stuff (quote semicolon) "Semicolon sends input")
  (sql-pop-to-buffer-after-send-region nil "Do not go to buffer when we send the input"))

(use-package sql-indent
  :hook (sqlind-minor-mode . (lambda ()
                               (setq sqlind-basic-offset 1)
                               (setq sqlind-indentation-offsets-alist
                                     `((select-clause sqlind-right-justify-clause)
                                       (insert-clause 0)
                                       (delete-clause 0)
                                       (update-clause 0)
                                       (select-table-continuation  sqlind-lineup-joins-to-anchor)
                                       ,@sqlind-default-indentation-offsets-alist)))))

;;; sql-conf.el ends here
