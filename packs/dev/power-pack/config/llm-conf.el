;;; llm-conf.el -- The LLM (AI) configuration -*- lexical-binding: t; -*-
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

(defun ar-emacs-gptel-ollama-endpoint ()
  "Compute the host:port pointing to the ollama server."
  (concat (or (getenv "EMACS_GPTEL_OLLAMA_HOST") "localhost")
          ":"
          (or (getenv "EMACS_GPTEL_OLLAMA_PORT") "11434")))

(defun ar-emacs-gptel-vllm-endpoint ()
  "Compute the host:port pointing to the vllm server."
  (concat (or (getenv "EMACS_GPTEL_VLLM_HOST") "localhost")
          ":"
          (or (getenv "EMACS_GPTEL_VLLM_PORT") "8000")))

(defun ar-emacs-gptel-llamacpp-endpoint ()
  "Compute the host:port pointing to the llama.cpp server."
  (concat (or (getenv "EMACS_GPTEL_LLAMACPP_HOST") "localhost")
          ":"
          (or (getenv "EMACS_GPTEL_LLAMACPP_PORT") "8000")))

(setq ar-emacs-gptel-backend-vllm
      (gptel-make-openai "vLLM"
        :protocol "http"
        :host (ar-emacs-gptel-vllm-endpoint)
        :header '(("Content-Type" . "application/json"))
        :stream t
        :models '((Qwen3-VL-32B
                   :description "Meet Qwen3-VL — the most powerful vision-language model in the Qwen series to date."
                   :capabilities (media json)
                   :mime-types ("application/pdf" "image/jpeg" "image/png" "image/gif" "image/webp")
                   :request-params (:top_p 0.8 :top_k 20 :temperature 1.0 :greedy :json-false
                                           :presence_penalty 2.0 :repetition_penalty 1.0
                                           :chat_template_kwargs (:enable_thinking :json-false))))))

(setq ar-emacs-gptel-backend-llamacpp
      (gptel-make-openai "llama.cpp"
        :protocol "http"
        :host (ar-emacs-gptel-llamacpp-endpoint)
        :header '(("Content-Type" . "application/json"))
        :stream t
        :models '(gpt-oss-120b glm-4.6V Qwen3-VL-32B)))

(defun ar-emacs--gptel-add-project-summary ()
  "Call gptel-add-file on PROJECT_SUMMARY.md if it is present in the project root."
  (let ((file-path (expand-file-name "PROJECT_SUMMARY.md" (projectile-project-root))))
    (when (file-exists-p file-path)
      (gptel-add-file file-path))))

(defun ar-emacs--gptel-add-code-style ()
  "Call gptel-add-file on LLM_CODE_STYLE.md if it is present in the project root."
  (let ((file-path (expand-file-name "LLM_CODE_STYLE.md" (projectile-project-root))))
    (when (file-exists-p file-path)
      (gptel-add-file file-path))))

(use-package gptel
  :commands (gptel gptel-menu gptel-rewrite gptel-send gptel-tools gptel-make-preset)
  :bind (:map gptel-mode-map
              ("C-c C-c" . gptel-abort)
              ("C-c C-g" . gptel-abort)
              ("C-c C-p" . gptel--preset))
  :hook
  (gptel-mode . (lambda () (olivetti-mode 1)))

  :custom
  ((gptel-default-mode 'markdown-mode "Use markdown as the default")
   (gptel-window-select t "Select the window after creation")
   (gptel-window-side 'right "Display on the right side")
   ;; https://github.com/karthink/gptel?tab=readme-ov-file#extra-org-mode-conveniences
   (gptel-org-branching-context t)
   (gptel-expert-commands t))

  :config
  ;; for mcp.el tools
  (require 'gptel-integrations)

  (progn (message "Running LLM exec-path-from-shell.")
         (exec-path-from-shell-copy-envs '("EMACS_GPTEL_VLLM_HOST"
                                           "EMACS_GPTEL_VLLM_PORT"
                                           "EMACS_GPTEL_LLAMA_PORT"
                                           "EMACS_GPTEL_LLAMA_PORT")))

  (setq ar-emacs-llm-prompts-dir (expand-file-name "llm/prompts" user-emacs-directory))

  (setq gptel-rewrite-directives-hook #'ar-emacs-gptel-rewrite-directives-hook)

  ;; Directives can be either local or loaded from files
  (setq gptel-directives
        (let ((markdown-directives (ar-emacs-gptel-load-all-markdown-directives ar-emacs-llm-prompts-dir)))
          `((default . nil)
            ,@markdown-directives)))

  (gptel-make-preset 'coder
    :description "A preset optimized for web searches"
    :track-media t
    :system (alist-get 'expert-developer gptel-directives)
    :tools '("ALL")
    :pre (lambda () (gptel-mcp-connect
                     (list "sequential-thinking" "duckduckgo" "fetch" "url-opener" "time" "filesystem-git"))))

  (gptel-make-preset 'emacsconfigurator
    :description "A preset optimized for modifying my emacs config"
    :system (alist-get 'emacs-configurator gptel-directives)
    :tools '(:append ("filesystem-emacs" "git-emacs"))
    :pre (lambda () (gptel-mcp-connect
                     (list "filesystem-emacs" "git-emacs"))))

  (gptel-make-preset 'gitassistant
    :description "A preset to assist with git commit messages, PRs and so on"
    :model 'claude-sonnet-4
    :system (alist-get 'git-assistant gptel-directives))

  (gptel-make-preset 'ocr
    :description "A preset to assist with OCR and binary to text extraction"
    :track-media t
    :system (alist-get 'nanonets-ocr gptel-directives))

  ;; https://github.com/karthink/gptel?tab=readme-ov-file#extra-org-mode-conveniences
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n"))

(use-package mcp
  :custom
  (mcp-log-level 'info)

  :init
  (setq mcp-hub-servers '())

  :config

  (setq mcp-hub-servers
        (append
         mcp-hub-servers
         `(("filesystem-git" . (:command
                                "podman"
                                :args ("run" "-i" "--rm" "--network=host"
                                       "--mount" ,(concat "type=bind,src=" ar-emacs-projects-dir ",dst=/projects")
                                       "mcp/filesystem"
                                       "/projects")))
           ("filesystem-emacs" . (:command
                                  "podman"
                                  :args ("run" "-i" "--rm" "--network=host"
                                         "--mount" ,(concat "type=bind,src=" ar-emacs-emacs-config-dir ",dst=/projects")
                                         "mcp/filesystem"
                                         "/projects")))
           ("git-emacs" . (:command
                           "uvx"
                           :args ("mcp-server-git"
                                  "--repository" ,ar-emacs-emacs-config-dir)
                           :env (:AR_PROMPT_GIT_DISABLED "true")))
           ("fetch" . (:command
                       "podman"
                       :args ("run", "-i", "--rm", "mcp/fetch")))
           ("duckduckgo" . (:command
                            "uvx"
                            :args ("duckduckgo-mcp-server")))
           ("url-opener" . (:command
                            "npx"
                            :args ("@world9/url-opener")))

           ("time" . (:command
                      "uvx"
                      :args ("mcp-server-time" "--local-timezone=Canada/Mountain")))
           ("sequential-thinking" . (:command
                                     "podman"
                                     :args ("run", "-i", "--rm", "mcp/sequentialthinking")
                                     :env (:DISABLE_THOUGHT_LOGGING true)))))))

;;;;;;;;;;;;;
;; Wingman ;;
;;;;;;;;;;;;;

(use-package wingman
  :bind (:map wingman-mode-prefix-map
         ("TAB" . wingman-fim-inline)
         :map wingman-mode-completion-transient-map
         ("TAB" . wingman-accept-full)
         ("S-TAB" . wingman-accept-line)
         ("M-S-TAB" . wingman-accept-word))

  :custom
  (wingman-auto-fim nil)
  (wingman-log-level 4)
  (wingman-ring-n-chunks 16)
  (wingman-llama-endpoint (concat "http://" (ar-emacs-gptel-llamacpp-endpoint) "/infill"))

  ;; assumes use of Modus Themes; substitute with preferred color scheme
  ;; (set-face-attribute 'wingman-overlay-face nil
                      ;; :foreground  (modus-themes-get-color-value 'red-warmer)
                      ;; :background  (modus-themes-get-color-value 'bg-inactive))

  ;; don't provide completions in files that typically contain secrets
  (add-to-list 'wingman-disable-predicates
               (lambda ()
                 (or (derived-mode-p 'envrc-file-mode)
                     (derived-mode-p 'direnv-envrc-mode)
                     (when buffer-file-name
                       (let ((fname (file-name-nondirectory buffer-file-name)))
                         (or (string-equal ".env" fname)
                             (string-equal ".envrc" fname)
                             (string-prefix-p ".localrc" fname))))))))

(use-package aider
  :config
  ;;
  ;; Main config taken from ~/.aider.conf.yml
  ;;
  ;; or use aider-transient-menu-2cols / aider-transient-menu-1col, for narrow screen
  (aider-magit-setup-transients) ;; add aider magit function to magit menu
  ;; auto revert buffer
  (global-auto-revert-mode 1)
  (auto-revert-mode 1))

;;; llm-conf.el ends here
