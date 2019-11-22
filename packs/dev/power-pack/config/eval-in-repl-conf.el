;;; eval-in-repl-conf.el --- EIR Config

;;; Commentary:

;;; Code:

(use-package eval-in-repl
  :custom
  (eir-jump-after-eval t "Always jump to REPL after eval")
  (eir-repl-placement 'right "Place the REPL on the left")
  (eir-ielm-eval-in-current-buffer t "Evaluate from current buffer"))

;;; eval-in-repl-conf.el ends here
