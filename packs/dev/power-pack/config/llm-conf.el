;;; llm-conf.el -- The LLM (AI) configuration
;;
;; Author: Andrea Richiardi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Custom configuration

;;; Code:

(defun ar-emacs-gptel-load-markdown-directive (file)
  "Load a gptel directive from a markdown FILE.
Returns a cons of (name . directive) where name is derived from filename
and directive is the content of the file."
  (condition-case err
      (let ((max-specpdl-size (* 10 max-specpdl-size)) ; Increase recursion limit
            (max-lisp-eval-depth (* 10 max-lisp-eval-depth))
            (large-file-warning-threshold nil) ; Disable large file warning
            (gc-cons-threshold (* 100 1024 1024))) ; 100MB for GC threshold
        (with-temp-buffer
          ;; Temporarily increase buffer size limit for this operation
          (let ((enable-local-variables nil)
                (buffer-read-only nil)
                (buffer-file-name nil)
                (max-mini-window-height 0.5))
            (insert-file-contents file)
            (let* ((filename (file-name-nondirectory file))
                   (name (intern (car (split-string filename "\\.md"))))
                   (content (buffer-substring-no-properties
                             (point-min)
                             (point-max))))
              (cons name (string-trim content))))))
    (error
     (message "Error loading directive from %s: %s"
              file (error-message-string err))
     nil)))

(defun ar-emacs-gptel-load-all-markdown-directives (directory)
  "Load all markdown files from DIRECTORY as gptel directives.
Returns a list of cons cells (name . directive) for each .md file."
  (when (file-directory-p directory)
    (let ((markdown-files (directory-files directory t "\\.md$")))
      (delq nil
            (mapcar #'ar-emacs-gptel-load-markdown-directive markdown-files)))))

(defun ar-emacs-gptel-rewrite-directives-hook ()
  "Compute the rewrite directive."
  (let* ((current-mode (symbol-name major-mode))
         (language (replace-regexp-in-string "-mode" "" current-mode)))
    (string-join
     (list (concat "Rewrite " (capitalize language) " code. Do not explain the reason of your changes.")
           "Do NOT use markdown backticks (```) to format your response. If you use LaTex notation, enclose math in \\( and \\), or \\[ and \\] delimiters.")
     "\n")))

(bind-keys :prefix-map ar-emacs-llm-prefix-map
           :prefix-docstring "Prefix key for all things LLM."
           :prefix "C-c C-x"
           ("a" . gptel)
           ("C-a" . gptel)
           ("s"   . gptel-send)
           ("C-s" . gptel-send)
           ("m"   . gptel-menu)
           ("C-m" . gptel-menu)
           ("r"   . gptel-rewrite)
           ("C-r" . gptel-rewrite))

(use-package gptel
  :commands (gptel gptel-menu gptel-rewrite gptel-send)

  :hook (gptel-mode . (lambda () (olivetti-mode 1)))

  :custom
  ((gptel-default-mode 'org-mode "Use org-mode as the default")
   (gptel-window-select t "Select the window after creation")
   (gptel-window-side 'right "Display on the right side")
   ;; https://github.com/karthink/gptel?tab=readme-ov-file#extra-org-mode-conveniences
   (gptel-org-branching-context t))


  :config
  (setq ar-emacs-llm-prompts-dir (expand-file-name "llm/prompts" user-emacs-directory))

  (setq gptel-model 'codestral:22b
        gptel-backend (gptel-make-ollama "Ollama"
                        :host (concat (or (getenv "EMACS_GPTEL_OLLAMA_HOST") "localhost")
                                      ":"
                                      (or (getenv "EMACS_GPTEL_OLLAMA_PORT") "11434"))
                        :stream t
                        :models '("codestral:22b"
                                  "qwen2.5-coder:32b")))

  (setq gptel-rewrite-directives-hook #'ar-emacs-gptel-rewrite-directives-hook)

  ;; Directives can be either local or loaded from files
  (setq gptel-directives
        (let ((markdown-directives (ar-emacs-gptel-load-all-markdown-directives ar-emacs-llm-prompts-dir)))
          `((default . ,(string-join
                         (list "To assist: be terse. Do not offer unprompted advice or clarifications. "
                               "Speak in specific, topic relevant terminology. Do NOT hedge or qualify. Speak directly and be willing to make creative guesses."
                               "Explain your reasoning but if you don’t know, say you don’t know. Be willing to reference less reputable sources for ideas."
                               "Never apologize.  Ask questions when unsure."
                               "Do NOT use markdown backticks (```) to format your response. If you use LaTex notation, enclose math in \\( and \\), or \\[ and \\] delimiters.")
                         "\n"))
            (programmer . ,(string-join
                            (list "You are a careful programmer. Provide code and only code as output without any additional text, prompt or note."
                                  "Do NOT use markdown backticks (```) to format your response.")
                            "\n"))
            (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, without any markdown code fences.")
            (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
            (explain . "Explain what this code does to a novice programmer. Do NOT use markdown backticks (```) to format your response.")
            (tutor . ,(string-join
                       (list "You are a tutor and domain expert in the domain of my questions. You will lead me to discover the answer myself by providing hints. Your instructions are as follows:"
                             "- If the question or notation is not clear to you, ask for clarifying details."
                             "- At first your hints should be general and vague."
                             "- If I fail to make progress, provide more explicit hints."
                             "- Never provide the answer itself unless I explicitly ask you to.  If my answer is wrong, again provide only hints to correct it."
                             "- If you use LaTeX notation, enclose math in \\( and \\) or \\[ and \\] delimiters.")
                       "\n"))
            ,@markdown-directives)))

  ;; https://github.com/karthink/gptel?tab=readme-ov-file#extra-org-mode-conveniences
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  )

;;; llm-conf.el ends here
