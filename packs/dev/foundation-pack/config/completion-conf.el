;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(live-add-pack-lib "company-mode")
(live-add-pack-lib "company-shell")

(require 'company)
(require 'company-shell)

(setq company-idle-delay 0)

(add-hook 'shell-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         '(company-shell company-shell-env))))
