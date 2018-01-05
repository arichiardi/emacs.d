;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(live-add-pack-lib "company-mode")
(live-add-pack-lib "company-shell")
(live-add-pack-lib "pos-tip")
(live-add-pack-lib "company-quickhelp")


(require 'company)
(require 'company-shell)
(require 'company-quickhelp)

(setq company-idle-delay 0)

(add-hook 'shell-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         '(company-shell company-shell-env))))

(eval-after-load "company-mode"
  (company-quickhelp-mode 1))
