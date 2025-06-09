;;; ar-emacs.el --- Some useful functions
;;; Commentary:
;;
;; Some useful functions
;;
;;; Code:

(require 'dash)

;; From: http://stackoverflow.com/questions/20041904/eclipse-like-line-commenting-in-emacs#answer-20064658
(defun ar-emacs-comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end))
  (forward-line))

;; From: http://www.bytopia.org/2014/11/26/rename-clojure-symbol-in/
(defun ar-emacs-narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens, otherwise it narrows.

   Intelligently means: region, subtree, or defun, whichever
   applies first.  With prefix P, don't widen, just narrow even
   if buffer is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

(defun ar-emacs-return (&optional arg)
  (interactive "P")
  (cond ((equal arg '-) ;; C--
         (beginning-of-line)
         (open-line 1))
        ((equal arg '(4)) ;; C-u
         (end-of-line)
         (open-line 1)
         (forward-line))
        (t ;; Otherwise
         (newline-and-indent))))

;; http://www.emacswiki.org/emacs/WindowResize
(defun ar-emacs-resize-window (&optional arg)    ; Hirose Yuuji and Bob Wiener
  "Resize window interactively."
  (interactive "p")
  (if (one-window-p) (error "Cannot resize sole window"))
  (or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
	(message
	 "h=heighten, s=shrink, w=widen, n=narrow (by %d);  1-9=unit, q=quit"
	 arg)
	(setq c (read-char))
	(condition-case ()
	    (cond
	     ((= c ?h) (enlarge-window arg))
	     ((= c ?s) (shrink-window arg))
	     ((= c ?w) (enlarge-window-horizontally arg))
	     ((= c ?n) (shrink-window-horizontally arg))
	     ((= c ?\^G) (keyboard-quit))
	     ((= c ?q) (throw 'done t))
	     ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
	     (t (beep)))
	  (error (beep)))))
    (message "Done.")))

(defun ar-emacs-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; needed for prelude-goto-symbol
(require 'imenu)

(defun ar-emacs-prelude-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido.

Optionally accepts an explicit SYMBOL-LIST."
  (interactive)
  (cond
   ((not symbol-list)
    (let (name-and-pos symbol-names position)
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (prelude-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (completing-read "Symbol? " (reverse symbol-names)))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))
      (recenter)))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (prelude-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names (substring-no-properties name))
          (add-to-list 'name-and-pos (cons (substring-no-properties name) position))))))))

(defun ar-emacs--cider-get-repl-buffer ()
  "Return a cider REPL buffer if one or prompt if many."
  (-when-let (repl-buffers (cider-repl-buffers))
    (if (= (length repl-buffers) 1)
        (car repl-buffers)
      (completing-read "Choose REPL buffer: "
                       (mapcar #'buffer-name repl-buffers)
                       nil t))))

(defun ar-emacs--clj-parse-sym-kw-map (bound-expr init-expr)
  "Parse an EDN string representing a symbol-keyword map and generate DEF forms.

BOUND-EXPR is a string like \"{task-id :id, task-canonical-id :canonical-id}\", and
INIT-EXPR is a string representing the variable holding the map value.

Returns a string containing the DEF forms for each symbol-keyword pair,
or nil if the map is empty."
  (let* ((hash (parseedn-read-str bound-expr))
         (keys (hash-table-keys hash)))
    (when (not (null keys))
      (let ((defs (mapcar (lambda (key)
                            (let ((value (gethash key hash)))
                              (format "(def %s (%s task))"
                                      (symbol-name key)
                                      (symbol-name value))))
                          keys)))
        (format "(let [sym# %s] %s)"
                init-expr
                (mapconcat 'identity defs " "))))))

(defun ar-emacs--clj-parse-keys-map (bound-expr init-expr)
  "Parse an EDN string representing a map with :keys and generate DEF forms.

BOUND-EXPR is a string like \"{\\:keys [task-id task-canonical-id]}\", and
INIT-EXPR is a string representing the variable holding the map value.

Returns a string containing the DEF forms for each key in the :keys
vector, or nil if the :keys vector is empty."
  (let* ((hash (parseedn-read-str bound-expr))
         (keys (gethash :keys hash)))
    (when keys
      (let ((defs (mapcar (lambda (key)
                            (format "(def %s (:%s task))" key key))
                          keys)))
        (format "(let [sym# %s] %s)"
                init-expr
                (mapconcat 'identity defs " "))))))

(defun ar-emacs--clj-bound-map-p (expr)
  "Check if EXPR is an EDN map by checking if it starts with '{'.

This is a simple heuristic and may not cover all valid EDN map formats,
but is sufficient for the use case of destructuring bindings."
  (string-prefix-p "{" expr))

(defun ar-emacs--clj-binding-to-form (bound-expr init-expr)
  "Evaluate destructuring bindings and generate DEF forms at the REPL.

BOUND-EXPR is a string representing a binding symbol or an EDN map.
INIT-EXPR is a string representing the value expression.

Returns a string containing the DEF form for the binding, or nil if the
binding is not a valid destructuring form."
  (when bound-expr
    (cond
     ((ar-emacs--clj-bound-map-p bound-expr)
      (or
       ;; Handle map with :keys destructuring
       (ar-emacs--clj-parse-keys-map bound-expr init-expr)
       ;; Handle symbol-keyword map destructuring
       (ar-emacs--clj-parse-sym-kw-map bound-expr init-expr)))
     ;; Fallback: simple binding
     (t
      (concat "(def " bound-expr " " init-expr ")")))))

(defun ar-emacs--clj-bindings-to-forms (bindings)
  "Parse destructuring bindings and generate forms.

BINDINGS is a list of symbols and expressions representing a let binding.

Returns a list of forms (as strings) containing the DEF forms for each binding."
  (when bindings
    (let* ((bound-expr (string-trim (pop bindings)))
           (init-expr (string-trim (pop bindings)))
           (form (ar-emacs--clj-binding-to-form bound-expr init-expr)))
      (cons form (ar-emacs--clj-bindings-to-forms bindings)))))

(defun ar-emacs-clj-eval-all-let-bindings ()
  "Evaluate all let bindings in the current buffer and send each (def ...)
form to the REPL.

Each form is sent individually, and any nil forms are skipped.  Errors
are re-thrown."
  (interactive)
  (condition-case err
      (let ((forms (ar-emacs--clj-bindings-to-forms (clojure--read-let-bindings))))
        (dolist (form forms)
          (when form
            (ar-emacs-send-to-repl form))))
    (error (signal (car err) (cdr err)))))

;; http://whattheemacsd.com/
(defun ar-emacs-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun ar-emacs-delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun ar-emacs-paredit-kill-parent-sexp (&optional arg)
  (interactive)
  (paredit-backward-up)
  (kill-sexp (or arg 1)))

;; http://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close
(defun ar-emacs-bury-compile-buffer (buffer string)
  "Bury a compilation BUFFER if succeeded without warnings "
  (let ((buffer-name (buffer-name buffer)))
    (if (and
         (when buffer-name (string-match "compilation" buffer-name))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            **(goto-char 1)**
            (search-forward "warning" nil t))))
        (run-with-timer 1 nil
                        (lambda (buf)
                          (bury-buffer buf)
                          (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                        buffer))))
(add-hook 'compilation-finish-functions 'ar-emacs-bury-compile-buffer)

;; http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(ignore-errors
  (require 'ansi-color)
  (defun ar-emacs-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'ar-emacs-colorize-compilation-buffer))

(defun ar-emacs-paredit-first-buffer-form (&optional arg)
  (interactive "P")
  (if current-prefix-arg
      (progn
        (goto-char (point-min))
        (paredit-forward))
    (goto-char (point-min))))

(defun ar-emacs-send-to-repl (form)
  "Send a FORM to the REPL for evaluation.

It detects if either Cider or Inf-Clojure are enabled for the
current buffer and makes the right choice."
  (interactive)
  (save-some-buffers)
  (cond
   ((or (bound-and-true-p cider-mode)
        (equal major-mode 'cider-repl-mode))
    (with-current-buffer (cider-current-repl "clj")
      (goto-char (point-max))
      (let ((beg (point)))
        (insert form)
        (indent-region beg (point)))
      (cider-repl-return)))

   ((or (bound-and-true-p inf-clojure-minor-mode)
        (equal major-mode 'inf-clojure-mode))
    (inf-clojure--send-string (inf-clojure-proc)
                              (inf-clojure--sanitize-command string)))

   (t (error "No Clojure mode enabled; use `cider-mode' or `inf-clojure-minor-mode'"))))

(defun ar-emacs-set-cljs-repl-type ()
  "Set the cider REPL type to cljs."
  (when (or (bound-and-true-p cider-mode)
        (equal major-mode 'cider-repl-mode))
   (with-current-buffer (cider-current-repl-buffer)
     (cider-repl-set-type "cljs")
     (cider-repl-return))))

(defun ar-emacs-cljs-piggieback-node-repl ()
  (interactive)
  (save-some-buffers)
  (ar-emacs-send-to-repl
   (concat "(require 'cljs.repl.node)\n"
           "(cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env))\n")))

(defun ar-emacs-cljs-node-repl ()
  (interactive)
  (save-some-buffers)
  (ar-emacs-send-to-repl
   (concat "(require '[cljs.repl :as repl])"
           "(require '[cljs.repl.node :as node])"
           "(repl/repl (node/repl-env))")))

(defun ar-emacs-cider-repl-in-ns-dev ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(require 'dev) (in-ns 'dev)")
    (cider-repl-return)))

(defun ar-emacs--copy-to-primary-no-white-space (s)
  "Copy S to the primary clipboard."
  (deactivate-mark)
  (gui-set-selection
    'PRIMARY
    (replace-regexp-in-string "[\s\n]" "" s)))

 (defun ar-emacs-sprunge ()
  "Posts the current buffer or region to sprunge."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (content (buffer-substring-no-properties start end)))
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`content\\'")
        (let ((filename "/tmp/sprunge-post"))
          (with-temp-buffer
            (let ((coding-system-for-read 'utf-8)
                  (coding-system-for-write 'utf-8))
              (insert content)
              (write-file filename)))
          (substring (shell-command-to-string (concat "curl -s -F 'sprunge=<" filename "' http://sprunge.us")) 0 -1)))

     (lambda (res)
       (ar-emacs--copy-to-primary-no-white-space res)
       (message res)))))

;; http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down

(defun ar-emacs-move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun ar-emacs-move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; plexus [3:16 PM]
;; Something I wish I had thought of earlier:
;; ```
;; (defun plexus-clojure-extras/cider-pprint-register (register)
;;   (interactive (list (register-read-with-preview "Eval register: ")))
;;   (cider--pprint-eval-form (get-register register)))
;; ```
;; Select some clojure code, then do `M-x copy-to-register` (`C-x r x`), and pick a letter
;; now with `M-x plexus-clojure-extras/cider-pprint-register <letter>` you can run that code from anywhere, and see the result in a pop-up buffer
;; spacemacs folks can do this to have it available as `,,`

(defun ar-emacs-cider-pprint-register (register)
  "Pretty print a REGISTER containing Clojure code."
  (interactive (list (register-read-with-preview "Eval register: ")))
  (cider--pprint-eval-form (get-register register)))

(defvar ar--emacs-gitidentities-dir "~/.gitidentities/")
(defvar ar--emacs-gitidentities-default "a.richiardi.work")

(defun ar--emacs--not-dotdirp (name)
  "Match if the NAME is either . or .."
  (not (and (string= "." name) (string= ".." name))))

(defun ar-emacs-git-insert-include ()
  "Choose the animal and takes the corresponding action.
Returns whatever the action returns."
  (interactive)
  (let ((choice (read-file-name
                 "Pick identity: "
                 ar--emacs-gitidentities-dir
                 ar--emacs-gitidentities-default
                 'require-match
                 nil
                 #'ar--emacs--not-dotdirp)))
    (if (string-blank-p choice)
        (message "No choice made, skipping.")
      (progn (message "Adding include for `%s'" choice)
               (insert "[include]")
               (newline-and-indent)
               (insert (format "path = %s" choice))
               (newline-and-indent)))))

(defvar ar-emacs--safe-project-dirs
  '()
  "List of project dirs that contain safe vars in .dir-locals.el.")

(defun ar-emacs--safe-project-dir-p (_)
  "Return t if the buffer file name belongs to a safe project dir.

  See `ar-emacs--safe-project-dirs' for a list of them.

  You can use it with something like:

    (add-hook 'prog-mode-hook
              (lambda ()
              (put 'prettier-js-command 'safe-local-variable #'ar-emacs--safe-project-dir-p)))"
  (seq-some
   (lambda (safe-dir)
     (string-match-p (regexp-quote safe-dir) (file-name-directory buffer-file-name)))
   ar-emacs--safe-project-dirs))

(defun ar-emacs-plantuml-jar-path ()
  "Retrieve the path to the plantuml.jar."
  ;; TODO: find a way to get the path in linux
  (when (eq system-type 'darwin)
    (->> "brew --prefix plantuml"
         (shell-command-to-string)
         (string-trim)
         (file-truename)
         (expand-file-name "libexec")
         (expand-file-name "plantuml.jar"))))

(defun ar-emacs-consult-ripgrep ()
  "Run `consult-ripgrep' using the selected region as the initial search string.
The search will be performed in the current project root (from
projectile, if available), or in the current directory otherwise."
  (interactive)
  (let* ((region (when (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))))
         (dir (if (fboundp 'projectile-project-root)
                  (projectile-project-root)
                default-directory)))
    (if region
        (consult-ripgrep dir region)
      (consult-ripgrep))))

(defun ar-emacs-consult-git-grep ()
  "Run `consult-git-grep' using the selected region as the initial search string.
The search will be performed in the current project root (from
projectile, if available), or in the current directory otherwise."
  (interactive)
  (let* ((region (when (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))))
         (dir (if (fboundp 'projectile-project-root)
                  (projectile-project-root)
                default-directory)))
    (if region
        (consult-git-grep dir region)
      (consult-ripgrep))))


(provide 'ar-emacs)

;;; ar-emacs.el ends here
