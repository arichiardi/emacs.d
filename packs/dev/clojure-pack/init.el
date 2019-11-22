;; Clojure Pack
(live-add-pack-lib "rainbow-delimiters")
(require 'rainbow-delimiters)
(require 'paredit)

(live-add-pack-lib "uuid")
(require 'uuid)

(live-add-pack-lib "edn")
(require 'edn)

(live-load-config-file "clojure-conf.el")
(live-load-config-file "cider-conf.el")
