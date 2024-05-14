;;; init.el --- Clojure Pack

;;; Commentary:

;;; Code:

(live-add-pack-lib "rainbow-delimiters")
(require 'rainbow-delimiters)
(require 'paredit)

(live-add-pack-lib "uuid")
(require 'uuid)

(live-load-config-file "clojure-conf.el")
(live-load-config-file "cider-conf.el")

(use-package babashka)
(use-package zprint
  :bind (:map clojure-mode-map
         ("C-c t z" . zprint)))

;;; init.el ends here
