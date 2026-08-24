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
            ar-emacs-home-tmp-dir
            ar-emacs-emacs-config-dir
            ar-emacs-llm-config-dir
            ar-emacs-llm-recipes-dir
            ar-emacs-llm-prompts-dir
            ar-emacs-llm-skills-dir))

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

(defun ar-emacs-gptel-image-model (model &optional description)
  "Return a gptel model spec for a multimodal model.
MODEL is the model name. DESCRIPTION is optional."
  (append (list model)
          (if description (list :description description) nil)
          (list :capabilities '(media json)
                :mime-types '("application/pdf" "image/jpeg" "image/png" "image/gif" "image/webp"))))

(defun ar-emacs-gptel-video-model (model &optional description)
  "Return a gptel model spec for a model with video and image capabilities.
MODEL is the model name. DESCRIPTION is optional."
  (append (list model)
          (if description (list :description description) nil)
          (list :capabilities '(media json)
                :mime-types '("application/pdf" "image/jpeg" "image/png" "image/gif" "image/webp" 
                              "video/mp4" "video/webm" "video/ogg" "video/quicktime"))))

(use-package gptel
  :commands (gptel gptel-menu gptel-rewrite gptel-send gptel-tools gptel-make-preset gptel-api-key-from-auth-source)
  :bind (:map gptel-mode-map
              ("<escape>". gptel-abort)
              ("C-g"     . gptel-abort)
              ("C-c C-c" . gptel-send)
              ("C-c C-q" . gptel-abort)
              ("C-c C-p" . gptel-preset))
  :hook
  (gptel-mode . (lambda ()
                  (olivetti-mode 1)
                  (company-mode-on)))

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
         (exec-path-from-shell-copy-envs '("EMACS_GPTEL_IKLLAMA_HOST"
                                           "EMACS_GPTEL_IKLLAMA_PORT"
                                           "EMACS_GPTEL_LLAMA_HOST"
                                           "EMACS_GPTEL_LLAMA_PORT"
                                           "EMACS_GPTEL_VLLM_HOST"
                                           "EMACS_GPTEL_VLLM_PORT"
                                           "LOCAL_ALBA_HOST"
                                           "LOCAL_ALBA_URL"
                                           "LOCAL_ALBA_TOKEN"
                                           "LOCAL_LLM_HOST"
                                           "LOCAL_SEARXNG_HOST"
                                           "LOCAL_SEARXNG_PORT"
                                           "MCP_POSTGRES_URL"
                                           "MCP_SEARCH_URL"
                                           "MCP_TEXTWEB_URL")))

  (setq gptel-rewrite-directives-hook #'ar-emacs-gptel-rewrite-directives-hook)

  (setq gptel--qwen-family-models
        `(,(ar-emacs-gptel-image-model 'Qwen3.6-27B-MTP "Qwen3.X represents a significant leap forward, integrating breakthroughs in multimodal learning, architectural efficiency, reinforcement learning scale, and global accessibility to empower developers and enterprises with unprecedented capability and efficiency.")
          ,(ar-emacs-gptel-image-model 'Qwen3.6-35B-A3B-MTP "Qwen3.X represents a significant leap forward, integrating breakthroughs in multimodal learning, architectural efficiency, reinforcement learning scale, and global accessibility to empower developers and enterprises with unprecedented capability and efficiency.")))

  (setq ar-emacs-gptel-backend-alba
        (gptel-make-openai "alba"
          :protocol "https"
          :host (exec-path-from-shell-getenv "LOCAL_ALBA_HOST")
          :endpoint "/api/v1/chat/completions"
          :key (exec-path-from-shell-getenv "LOCAL_ALBA_TOKEN")
          :stream t
          :models gptel--qwen-family-models))

  (setq ar-emacs-gptel-backend-openrouter
        (gptel-make-openai "openrouter"
          :protocol "https"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key 'gptel-api-key-from-auth-source
          :models `(,(ar-emacs-gptel-image-model 'google/gemma-4-26b-a4b-it)
                    ,(ar-emacs-gptel-image-model 'google/gemma-4-31b-it)
                    ,(ar-emacs-gptel-video-model 'qwen/qwen3.5-122b-a10b)
                    ,(ar-emacs-gptel-image-model 'qwen/qwen3.6-27b "Qwen3.X represents a significant leap forward, integrating breakthroughs in multimodal learning, architectural efficiency, reinforcement learning scale, and global accessibility to empower developers and enterprises with unprecedented capability and efficiency.")
                    ,(ar-emacs-gptel-image-model 'qwen/qwen3.8-27b "Qwen3.X represents a significant leap forward, integrating breakthroughs in multimodal learning, architectural efficiency, reinforcement learning scale, and global accessibility to empower developers and enterprises with unprecedented capability and efficiency.")
                    poolside/laguna-s-2.1:free
                    poolside/laguna-xs-2.1:free)))

  ;; Directives can be either local or loaded from files
  (setq gptel-directives
        (let ((markdown-directives (ar-emacs-gptel-load-all-markdown-directives ar-emacs-llm-prompts-dir)))
          `((default . nil)
            ,@markdown-directives)))

  (defvar ar-emacs-developer-mcps
    '("sequential-thinking" "searxNcrawl-local" "fetch" "time"
      "workspace-filesystem" "shell-in-projects" "shell-in-config"))

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
    :pre   (lambda () (gptel-mcp-connect ar-emacs-developer-mcps t)i)
    :tools '(:eval (ar-emacs-mcp-tool-names ar-emacs-developer-mcps)))

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
    :parents 'developer
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
                      1.0
                      :top_p 0.95
                      :top_k 20
                      :min_p 0.0
                      :presence_penalty 1.5 :repetition_penalty 1.0
                      :chat_template_kwargs (:enable_thinking t)))

  (gptel-make-preset 'ocr
    :description "A preset to assist with OCR and binary to text extraction"
    :track-media t
    :system (alist-get 'ocr gptel-directives))

  ;; https://github.com/karthink/gptel?tab=readme-ov-file#extra-org-mode-conveniences
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n"))

(use-package mcp
  :defines (mcp-hub-servers)
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
                                   :args ("--with" "mcp<2.0" "cli-mcp-server")
                                   :env (:ALLOWED_DIR ,ar-emacs-projects-dir
                                         :ALLOWED_COMMANDS "ls,find,tree,cat,pwd,tail,head,sed,tr,wc,mkdir,date,echo,timeout,ssh,git,gpg,gh,hub,ag,rg,make,clojure,clj-nrepl-eval,clj-paren-repair,clj-kondo,cljfmt,psql"
                                         :ALLOWED_FLAGS    "all"
                                         :MAX_COMMAND_LENGTH "2048"
                                         :COMMAND_TIMEOUT    60
                                         :ALLOW_SHELL_OPERATORS "true")))
           ("shell-in-config" . (:command
                                   "uvx"
                                   :args ("--with" "mcp<2.0" "cli-mcp-server")
                                   :env (:ALLOWED_DIR ,ar-emacs-home-config-dir
                                         :ALLOWED_COMMANDS "ls,find,tree,cat,pwd,tail,head,wc,date,echo,timeout,ssh,git,gpg,gh,hub,ag,rg,make,clojure,clj-kondo,cljfmt,psql"
                                         :ALLOWED_FLAGS    "-l,-a,--help,--version"
                                         :MAX_COMMAND_LENGTH "2048"
                                         :COMMAND_TIMEOUT  5
                                         :ALLOW_SHELL_OPERATORS "false")))
           ("workspace-filesystem" . (:command "npx"
                                               :args ("-y" "--package" "zod@^4" "--package" "@modelcontextprotocol/server-filesystem" "mcp-server-filesystem")
                                               :roots ((:uri ,(concat "file://" ar-emacs-projects-dir) :name "Projects")
                                                       (:uri ,(concat "file://" ar-emacs-home-tmp-dir) :name "Home Tmp")
                                                       (:uri ,(concat "file://" ar-emacs-emacs-config-dir) :name "Emacs Config")
                                                       (:uri ,(concat "file://" ar-emacs-llm-skills-dir) :name "Skills"))))
           ("git-emacs" . (:command
                           "uvx"
                           :args ("mcp-server-git"
                                  "--repository" ,ar-emacs-emacs-config-dir)
                           :env (:AR_PROMPT_GIT_DISABLED "true")))
           ("fetch" . (:command
                       "podman"
                       :args ("run", "-i", "--rm", "mcp/fetch")))
           ("postgres-mcp" . (:url
                              ,(getenv "MCP_POSTGRES_URL")
                              :timeout 120))
           ("searxng-local" . (:command
                               "podman"
                               :args ("run" "-i" "--rm" "--network=host" "-e" "SEARXNG_URL"
                                      "isokoliuk/mcp-searxng:latest")
                               :env (:SEARXNG_URL ,(concat "http://" (getenv "LOCAL_SEARXNG_HOST")
                                                           ":" (getenv "LOCAL_SEARXNG_PORT")))))
           ("searxNcrawl-mcp" . (:url
                                 ,(getenv "MCP_SEARCH_URL")
                                 :timeout 120))
           ("textweb-mcp" . (:url
                             ,(getenv "MCP_TEXTWEB_URL")
                             :timeout 120))
           ("sequential-thinking" . (:command
                                     "npx"
                                     :args ("-y", "@modelcontextprotocol/server-sequential-thinking")
                                     :env (:DISABLE_THOUGHT_LOGGING true)))
           ("time" . (:command
                      "uvx"
                      :args ("mcp-server-time" "--local-timezone=Canada/Mountain")))
           ("url-opener" . (:command
                            "npx"
                            :args ("@world9/url-opener")))))))

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
