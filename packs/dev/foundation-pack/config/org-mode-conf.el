;;; org-mode-conf.el --- Org Mode Config

;;; Commentary:

;;; Code:

(use-package org-protocol
  :demand t)

(use-package ob-clojure)
(use-package ob-mongo)

(defun ar-emacs--get-open-org-file ()
  "Asks to the user to select one of the `org-mode' buffers."
  (buffer-file-name
   (get-buffer
    (org-icompleting-read "Buffer: " (mapcar 'buffer-name (org-buffer-list 'files))))))

(defun ar-emacs--org-get-x-clipboard ()
  "Return the values of the X."
  (delq nil
	(list (org-get-x-clipboard 'PRIMARY)
	      (org-get-x-clipboard 'CLIPBOARD)
	      (org-get-x-clipboard 'SECONDARY))))

(defun ar-emacs--org-retrieve-commit-text ()
  "Return INITIAL or try to call git-commit-buffer-message."
  (cond
   ((let ((captured (org-capture-get :initial)))
      (when captured captured)))

   ((fboundp 'git-commit-buffer-message)
    (with-current-buffer (org-capture-get :original-buffer)
      (git-commit-buffer-message)))

   ;; The following does not work because org-capture modifies the clipboard
   ;; before inserting an entry.
   ;;
   ;; ((let ((primary (org-get-x-clipboard 'PRIMARY)))
   ;;    (when primary primary)))
   ;;
   ;; ((let ((clipboard (org-get-x-clipboard 'CLIPBOARD)))
   ;;     (when clipboard clipboard)))

   ((t nil))))

(defun ar-emacs--org-find-commit-ticket (text)
  "Find the ticket number from the input TEXT.

Basically it returns the string within square brackets."
  (when (string-match "\\[\\(.*\\)]" text)
    (match-string 1 text)))

(defun ar-emacs--org-find-commit-msg (text)
  "Find the actual commit message from the input TEXT.

Basically it returns the string after square brackets."
  (when (string-match "\\[.*\\]\\s-+\\(\\(.\\|\n\\)*\\)" text)
    (match-string 1 text)))

(defun ar-emacs--org-auto-todo-state-change ()
  "Automatically clock in or clock our on STARTED state change."
  (cond ((string-equal org-state "STARTED") (org-clock-in))
        ((and (not (string-equal org-state "STARTED"))
              (org-clock-is-active))
         (org-clock-out))))

;; http://article.gmane.org/gmane.emacs.orgmode/3629
(defvar ar-emacs--org-archive-expiry-days 7
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

(defun ar-emacs--org-no-cua-region-return (&optional arg)
  "Disable CUA region when in org mode, accepting an ARG."
  (interactive "P")
  (if (eq major-mode 'org-mode)
      (org-insert-heading-respect-content arg)
    (cua-rectangle-mark-mode arg)))

;; no time for transforming this to use-package at the moment
(eval-after-load "org"
  '(progn
     ;; From: https://stackoverflow.com/questions/36474810/turn-rectangle-mark-mode-off-in-org-mode-in-emacs
     (define-key cua-global-keymap (kbd "C-<return>") #'ar-emacs--org-no-cua-region-return)

     (define-prefix-command 'org-todo-state-map)
     (define-key org-mode-map "\C-cx" 'org-todo-state-map)

     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "h"
       #'(lambda nil (interactive) (org-todo "HOLD")))
     (define-key org-todo-state-map "n"
       #'(lambda nil (interactive) (org-todo "NEXT")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "NOT_FIXING")))))

(use-package org
  :mode ("\\.org\\'" "\\.org.gpg\\'")
  :hook
  (org-mode . org-indent-mode)
  (org-mode . flyspell-mode)
  (org-after-todo-state-change . ar-emacs--org-auto-todo-state-change)

  :config
  ;; http://orgmode.org/worg/org-configs/org-customization-guide.html
  ;; https://github.com/robertutterback/config/blob/master/emacs/org-mode.org
  ;; http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner/
  (setq org-directory (concat "~/git/" "ar-org"))
  (setq org-default-notes-file (concat org-directory "/agenda/notes.org.gpg"))
  (setq org-archive-location (concat org-directory "/archive/archive.org.gpg" "::datetree/")) ;; Filename::heading
  (setq org-agenda-files (list (concat org-directory "/agenda/notes.org.gpg")
                               (concat org-directory "/agenda/work.org.gpg")
                               (concat org-directory "/agenda/home.org.gpg")))

  (setq org-agenda-include-diary t)
  (setq org-reverse-note-order t)
  (setq org-return-follows-link t)
  (setq org-log-done t)
  (setq org-fast-tag-selection-single-key 'expert)
  (setq org-catch-invisible-edits 'show)
  ;; http://stackoverflow.com/a/27048241
  (setq org-loop-over-headlines-in-active-region 'start-level)

  ;;;;;;;;;;;;;;
  ;;  Refile  ;;
  ;;;;;;;;;;;;;;
  (setq org-refile-targets '((ar-emacs--get-open-org-file . (:maxlevel . 2))))
  (setq org-refile-use-outline-path 'file)
  (setq org-completion-use-ido t)

  ;;;;;;;;;;;
  ;; Babel ;;
  ;;;;;;;;;;;
  (org-babel-do-load-languages
   'org-babel-load-languages (quote ((emacs-lisp . t)
                                     (sqlite . t)
                                     (clojure . t)
                                     (mongo . t)
                                     (python . t))))

  ;;;;;;;;;;;;
  ;;  TAGS  ;;
  ;;;;;;;;;;;;

  (setq org-use-tag-inheritance t)

  ;;;;;;;;;;;;;;;;
  ;;  KEYWORDS  ;;
  ;;;;;;;;;;;;;;;;

  (setq org-log-into-drawer "LOGBOOK")

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
                (sequence "|" "NOT_FIXING(f)"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "light green" :weight bold)
                ("MEETING" :foreground "dark yellow" :weight bold)
                ("NEXT" :foreground "white" :weight bold)
                ("STARTED" :foreground "yellow" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "dark gray" :weight bold)
                ("NOT_FIXING" :foreground "dark gray" :weight bold))))

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING" . t) ("HOLD" . t))
                ("DONE" ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("STARTED" ("WAITING") ("CANCELLED") ("HOLD") ("NEXT"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  )

(use-package org-capture
  :config
  (setq org-capture-templates
        `(("a" "Article/video to read/watch"
           entry
           (file "")
           "* %U\n%?\n" :clock-resume t)

          ("t" "Todo"
           entry
           (file "")
           ,(string-join
             (list "* TODO %^{Brief Description} %^g"
                   ":LOGBOOK:"
                   ":added: %U"
                   ":END:"
                   "%?")
             "\n")
           :clock-resume t)

          ("b" "Blog idea"
           entry
           (file "")
           "* TODO %^{Brief Description}\n%?\n" :clock-resume t)

          ("e" "Email"
           entry
           (file "")
           "* TODO %^{Title}\n  Source: %u, %c\n\n  %i" :clock-in t :clock-resume t)

          ("n" "Note, snippet, word or fact"
           entry
           (file "")
           ,(string-join
             (list "* %? :note:"
                   "%a"
                   ":LOGBOOK:"
                   ":added: %U"
                   ":END:"
                   "%?")
             "\n")  :clock-in t :clock-resume t)

          ("w" "Work Templates")

          ("wt" "Work Todo"
           entry
           (file ,(concat org-directory "/agenda/work.org.gpg"))
           "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" :clock-resume t)

          ("wc" "Commit Ticket"
           entry
           (file+olp+datetree ,(concat org-directory "/agenda/tickets.org.gpg"))
           ,(string-join
             (list "* %(ar-emacs--org-find-commit-ticket (ar-emacs--org-retrieve-commit-text)) %^g"
                   ":LOGBOOK:"
                   ":added: %T"
                   ":END:"
                   "%(ar-emacs--org-find-commit-msg (ar-emacs--org-retrieve-commit-text))%?")
             "\n")
           :clock-resume t
           :tree-type week)

          ("wm" "Meeting/Call"
           entry
           (file ,(concat org-directory "/agenda/work.org.gpg"))
           "* %^{Name} :meeting:\n%?\n" :clock-in t :clock-resume t)

          ("wr" "Respond to"
           entry
           (file "")
           "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
          ))
  )

(use-package org-clock
  :init
  (setq org-clock-persist-file (concat live-current-pack-dir "org-clock-save.el"))

  :config
  (setq org-clock-in-resume t)
  (setq org-clock-persist 'history)
  (setq org-clock-report-include-clocking-task t)
  (setq org-clock-persistence-insinuate 'history)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-sound t)
  (setq org-clock-history-length 23)
  (setq org-clock-into-drawer t)
  ;; (setq org-clock-persist-query-resume nil)
  ;; (setq org-clock-in-switch-to-state "STARTED")
  )

(use-package org-agenda
  :config
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-ndays 7)
  (setq org-deadline-warning-days 14)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-start-on-weekday 1)

  (setq org-agenda-custom-commands
        `(("d" todo "DONE|CANCELLED" nil)
          ("w" todo "WAITING" nil)
          ("P" agenda "Today's Priorities"
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
            (org-agenda-ndays 1)
            (org-agenda-overriding-header "Today's Priority #A tasks: ")))
          ("c" "Weekly Commit Tickets"
           ((agenda "" ((org-agenda-files (list ,(concat org-directory "/agenda/tickets.org.gpg")))
                        (org-agenda-span 'week)
                        (org-agenda-start-on-weekday 1)
                        (org-agenda-overriding-header "Worked on tickets: ")
                        (org-agenda-time-grid nil)))))
          ("u" alltodo ""
           ((org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                         (quote regexp) "\n]+>")))
            (org-agenda-overriding-header "Unscheduled TODO entries: ")))
          ("A" agenda "" ((org-agenda-ndays 21))))))

;;; org-mode-conf.el ends here
