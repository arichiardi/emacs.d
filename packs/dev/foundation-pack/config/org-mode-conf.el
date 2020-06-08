;;; org-mode-conf.el --- Org Mode Config

;;; Commentary:

;;; Code:

(use-package ob-clojure)
(use-package ob-mongo)

(org-babel-do-load-languages
 'org-babel-load-languages (quote ((emacs-lisp . t)
                                   (sqlite . t)
                                   (clojure . t)
                                   (mongo . t)
                                   (python . t))))

;;; org-mode-conf.el ends here
