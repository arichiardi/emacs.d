;;; llm-conf.el -- The LLM (AI) configuration -*- lexical-binding: t; -*-
;;
;; Author: Andrea Richiardi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Custom configuration

;;; Code:

(use-package ar-emacs
  :defines (ar-emacs-projects-dir
            ar-emacs-home-config-dir
            ar-emacs-emacs-config-dir
            ar-emacs-llm-config-dir
            ar-emacs-llm-recipes-dir
            ar-emacs-llm-prompts-dir))

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

(defun ar-emacs-gptel-llamacpp-endpoint ()
  "Compute the host:port pointing to the llama.cpp server."
  (concat (or (getenv "EMACS_GPTEL_LLAMACPP_HOST") "localhost")
          ":"
          (or (getenv "EMACS_GPTEL_LLAMACPP_PORT") "10434")))

(defun ar-emacs-gptel-ikllama-endpoint ()
  "Compute the host:port pointing to the ollama server."
  (concat (or (getenv "EMACS_GPTEL_IKLLAMA_HOST") "localhost")
          ":"
          (or (getenv "EMACS_GPTEL_IKLLAMA_PORT") "11434")))

(defun ar-emacs-gptel-vllm-endpoint ()
  "Compute the host:port pointing to the vllm server."
  (concat (or (getenv "EMACS_GPTEL_VLLM_HOST") "localhost")
          ":"
          (or (getenv "EMACS_GPTEL_VLLM_PORT") "12434")))

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
              ("<escape>". gptel-abort)
              ("C-g"     . gptel-abort)
              ("C-c C-c" . gptel-send)
              ("C-c C-q" . gptel-abort)
              ("C-c C-p" . gptel--preset))
  :hook
  (gptel-mode . (lambda () (olivetti-mode 1)))

  :custom
  ((gptel-default-mode 'markdown-mode)
   (gptel-window-select t "Select the window after creation")
   (gptel-window-side 'right "Display on the right side")
   ;; https://github.com/karthink/gptel?tab=readme-ov-file#extra-org-mode-conveniences
   (gptel-org-branching-context t))

  :config
  (setq gptel-expert-commands t)

  ;; for mcp.el tools
  (require 'gptel-integrations)

  (progn (message "Running LLM exec-path-from-shell.")
         (exec-path-from-shell-copy-envs '("EMACS_GPTEL_VLLM_HOST"
                                           "EMACS_GPTEL_VLLM_PORT"
                                           "EMACS_GPTEL_LLAMA_HOST"
                                           "EMACS_GPTEL_LLAMA_PORT"
                                           "EMACS_GPTEL_IKLLAMA_HOST"
                                           "EMACS_GPTEL_IKLLAMA_PORT"
                                           "LOCAL_SEARXNG_HOST"
                                           "LOCAL_SEARXNG_PORT")))

  (setq gptel-rewrite-directives-hook #'ar-emacs-gptel-rewrite-directives-hook)

  (setq ar-emacs-gptel-backend-ikllama
        (gptel-make-openai "ik_llama"
          :protocol "http"
          :host (ar-emacs-gptel-ikllama-endpoint)
          :header '(("Content-Type" . "application/json"))
          :stream t
          :models '(gpt-oss-120B GLM-4.6V Qwen3.X-27B)))

  (setq ar-emacs-gptel-backend-llamacpp
        (gptel-make-openai "llama.cpp"
          :protocol "http"
          :host (ar-emacs-gptel-llamacpp-endpoint)
          :header '(("Content-Type" . "application/json"))
          :stream t
          :models '(gpt-oss-120B Qwen3-VL-32B Qwen3.X-27B)))

  (setq ar-emacs-gptel-backend-vllm
        (gptel-make-openai "vLLM"
          :protocol "http"
          :host (ar-emacs-gptel-vllm-endpoint)
          :header '(("Content-Type" . "application/json"))
          :stream t
          :models '((Qwen3.X-27B
                     :description "Qwen3.X represents a significant leap forward, integrating breakthroughs in multimodal learning, architectural efficiency, reinforcement learning scale, and global accessibility to empower developers and enterprises with unprecedented capability and efficiency."
                     :capabilities (media json)
                     :mime-types ("application/pdf" "image/jpeg" "image/png" "image/gif" "image/webp")
                     ;; NOTE: moved to presets
                     ;; :request-params (:temperature 1.0 :top_p 0.95 :top_k 20 :min_p 0.0
                     ;;                  :presence_penalty 0.0 :repetition_penalty 1.0
                     ;;                  :chat_template_kwargs (:enable_thinking :json-false))
                     ))))

  ;; Directives can be either local or loaded from files
  (setq gptel-directives
        (let ((markdown-directives (ar-emacs-gptel-load-all-markdown-directives ar-emacs-llm-prompts-dir)))
          `((default . nil)
            ,@markdown-directives)))

  (gptel-make-preset 'developer
    :description "A preset optimized for coding tasks"
    :track-media t
    :system (alist-get 'developer gptel-directives)
    :request-params '(:temperature
                      0.6
                      :top_p 0.95
                      :top_k 20
                      :min_p 0.0
                      :presence_penalty 0.0 :repetition_penalty 1.0
                      :chat_template_kwargs (:enable_thinking t :preserve_thinking t))
    :pre (lambda () (gptel-mcp-connect
                     (list "sequential-thinking" "searxng-local" "fetch"
                           "url-opener" "time" "shell-in-projects" "shell-in-config"))))

  (gptel-make-preset 'one-shot
    :description "A preset optimized for coding tasks that are one shot (like a simple rewrite)."
    :request-params '(:temperature
                      0.7
                      :top_p 0.95
                      :top_k 20
                      :min_p 0.0
                      :presence_penalty 1.5 :repetition_penalty 1.0
                      :chat_template_kwargs (:enable_thinking :json-false))
    :system (alist-get 'developer gptel-directives))

  (gptel-make-preset 'clojure-coder
    :description "A preset optimized for clojure coding tasks."
    :parents ["developer"]
    :system (alist-get 'clojure-coder gptel-directives))

  (gptel-make-preset 'elisper
    :description "A preset optimized for modifying my emacs config."
    :system (alist-get 'elisp-expert gptel-directives)
    :pre (lambda () (gptel-mcp-connect
                     (list "filesystem-emacs" "git-emacs"))))

  (gptel-make-preset 'git
    :description "A preset to assist with git operation against my projects."
    :system (alist-get 'developer gptel-directives)
    :post (lambda ()
            (gptel-mcp-connect
             (list "github" "sequential-thinking" "shell-in-projects"))))

  (gptel-make-preset 'git-commit-writer
    :description "A preset to assist with git commit messages, PRs and so on."
    :system (alist-get 'git-commit-writer gptel-directives)
    :request-params '(:temperature
                      0.7
                      :top_p 0.95
                      :top_k 20
                      :min_p 0.0
                      :presence_penalty 1.5 :repetition_penalty 1.0
                      :chat_template_kwargs (:enable_thinking :json-false)))

  (gptel-make-preset 'ocr
    :description "A preset to assist with OCR and binary to text extraction"
    :track-media t
    :system (alist-get 'ocr gptel-directives))

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
         `(("shell-in-projects" . (:command
                                   "uvx"
                                   :args ("cli-mcp-server")
                                   :env (:ALLOWED_DIR ,ar-emacs-projects-dir
                                                      :ALLOWED_COMMANDS "ls,find,tree,cat,pwd,tail,head,sed,tr,wc,mkdir,date,echo,ssh,git,gpg,gh,hub,ag,rg,make,clojure,clj-nrepl-eval,clj-paren-repair,clj-kondo,cljfmt"
                                                      :ALLOWED_FLAGS    "all"
                                                      :MAX_COMMAND_LENGTH "2048"
                                                      :COMMAND_TIMEOUT    60
                                                      :ALLOW_SHELL_OPERATORS "true")))
           ("shell-in-config" . (:command
                                   "uvx"
                                   :args ("cli-mcp-server")
                                   :env (:ALLOWED_DIR ,ar-emacs-home-config-dir
                                                      :ALLOWED_COMMANDS "ls,find,tree,cat,pwd,tail,head,wc,date,echo,ssh,git,gpg,gh,hub,ag,rg,make,clojure,clj-kondo,cljfmt"
                                                      :ALLOWED_FLAGS    "-l,-a,--help,--version"
                                                      :MAX_COMMAND_LENGTH "2048"
                                                      :COMMAND_TIMEOUT  5
                                                      :ALLOW_SHELL_OPERATORS "false")))
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
           ("searxng-local" . (:command
                               "podman"
                               :args ("run" "-i" "--rm" "--network=host" "-e" "SEARXNG_URL"
                                      "isokoliuk/mcp-searxng:latest")
                               :env (:SEARXNG_URL ,(concat "http://" (getenv "LOCAL_SEARXNG_HOST")
                                                           ":" (getenv "LOCAL_SEARXNG_PORT")))))
           ("searxNcrawl-local" . (:url ,(or (getenv "MCP_SEARCH_URL")
                                             (concat "http://" (getenv "LOCAL_SERVER_HOST") ":59555/mcp"))))
           ("sequential-thinking" . (:command
                                     "podman"
                                     :args ("run", "-i", "--rm", "mcp/sequentialthinking")
                                     :env (:DISABLE_THOUGHT_LOGGING true)))))))

(use-package agent-shell
  :bind (:map agent-shell-mode-map
              ("RET" . newline)
              ("C-c C-c" . shell-maker-submit)
              ("C-c C-q" . agent-shell-interrupt))

  :custom
  (agent-shell-agent-configs '(mistral-vibe goose))
  (agent-shell-preferred-agent-config (agent-shell-goose-make-agent-config))
  (agent-shell-session-strategy 'new)

  :config

  ;;;;;;;;;;;
  ;; goose ;;
  ;;;;;;;;;;;
  (setq agent-shell-goose-environment
        (agent-shell-make-environment-variables
         :inherit-env t
         "GOOSE_RECIPE_PATH" ar-emacs-llm-recipes-dir
         "CONTEXT_FILE_NAMES" "[\".goosehints\", \"AGENTS.md\"]"))

  (setq agent-shell-goose-authentication
        (agent-shell-make-goose-authentication :openai-api-key "<dummy>")))

;;; llm-conf.el ends here
