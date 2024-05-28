(use-package flyspell
  :config
  (progn
    ;; run flyspell with aspell, not ispell
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra"))
    (setq ispell-list-command "--list")))
