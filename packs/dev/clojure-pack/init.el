;;; init.el --- Clojure Pack

;;; Commentary:

;;; Code:

(live-add-pack-lib "rainbow-delimiters")
(require 'rainbow-delimiters)
(require 'paredit)

(live-add-pack-lib "uuid")
(require 'uuid)

(live-add-pack-lib "edn")
(require 'edn)

(live-load-config-file "clojure-conf.el")
(live-load-config-file "cider-conf.el")

(use-package clj-refactor
  :config
  (setq cljr-magic-require-namespaces (append '(("s" . "clojure.spec.alpha") ("ig" . "integrant.core") ("edn"   . "clojure.edn"))))
  (setq cljr-project-clean-exceptions (append '("deps.edn" "build.clj")))
  :custom
  (cljr-auto-clean-ns nil "Better to leave this off for now")
  (cljr-favor-prefix-notation t)
  (cljr-clojure-test-declaration "[clojure.test :as test :refer [deftest testing is]]")
  (cljr-magic-requires :prompt))

;;; init.el ends here
