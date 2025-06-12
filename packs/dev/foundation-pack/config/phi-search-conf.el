;;; phi-search-conf.el --- Phi-search config -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package phi-search
  :init
  (setq phi-search-limit           10000)
  (setq phi-search-case-sensitive 'guess)

  :config
  (set-face-attribute 'phi-search-selection-face nil
                      :background "#08161a")
  (set-face-attribute 'phi-search-match-face nil
                      :background "#194854")

  :bind (:map phi-search-default-map
              ("<prior>" . phi-search-again-or-previous)
              ("<next>" . phi-search-again-or-next)))

(use-package phi-replace)

;;; phi-search-conf.el ends here
