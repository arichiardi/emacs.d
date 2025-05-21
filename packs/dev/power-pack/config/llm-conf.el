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

(defun ar-emacs-gptel-ollama-endpoint ()
  "Compute the host:port pointing to the ollama server."
  (concat (or (getenv "EMACS_GPTEL_OLLAMA_HOST") "localhost")
          ":"
          (or (getenv "EMACS_GPTEL_OLLAMA_PORT") "11434")))

(defun ar-emacs-gptel-vllm-endpoint ()
  "Compute the host:port pointing to the ollama server."
  (concat (or (getenv "EMACS_GPTEL_VLLM_HOST") "localhost")
          ":"
          (or (getenv "EMACS_GPTEL_VLLM_PORT") "8000")))

(setq ar-emacs-gptel-backend-ollama
      (gptel-make-ollama "Ollama"
        :host (ar-emacs-gptel-ollama-endpoint)
        :stream t
        :models '((qwen2.5-coder:32b-instruct-q6_K
                   :description "The latest series of Code-Specific Qwen models, with significant improvements in code generation, code reasoning, and code fixing."
                   :request-params (:options (:num_ctx 32768 :min_p 0.1)))
                  (phi4:14b-q8_0
                   :description "Phi-4 is a 14B parameter, state-of-the-art open model from Microsoft."
                   :request-params (:options (:num_ctx 16384 :min_p 0.1)))
                  (deepseek-coder-v2:16b-lite-instruct-q4_K_M
                   :description "An open-source Mixture-of-Experts code language model that achieves performance comparable to GPT4-Turbo in code-specific tasks."
                   :request-params (:options (:num_ctx 65536 :min_p 0.1))))))

(setq ar-emacs-gptel-backend-vllm
      (gptel-make-openai "vLLM"
        :protocol "http"
        :host (ar-emacs-gptel-vllm-endpoint)
        :header '(("Content-Type" . "application/json"))
        :stream t
        :models '((QwQ-32B
                   :description "QwQ is the reasoning model of the Qwen series. Compared with conventional instruction-tuned models, QwQ, which is capable of thinking and reasoning, can achieve significantly enhanced performance in downstream tasks, especially hard problems."
                   ;; From https://huggingface.co/Qwen/QwQ-32B-AWQ
                   :request-params (:top_p 0.95 :top_k 40 :min_p 0.1
                                    :temperature 0.6 :repeat-penalty 1.1
                                    :add_generation_prompt "true"))
                  (Qwen3-30B
                   :description "Qwen3 is the large language model series developed by Qwen team, Alibaba Cloud."
                   ;; https://huggingface.co/karuko24/Qwen3-30B-A3B-W4A16
                   :request-params (:top_p 0.95 :top_k 40 :min_p 0.1
                                           :temperature 0.6 :repeat-penalty 1.1
                                           :add_generation_prompt "true"))
                  (DeepSeek-R1-Distill-Qwen-32B
                   :description "We introduce our first-generation reasoning model DeepSeek-R1. DeepSeek-R1 achieves performance comparable to OpenAI-o1 across math, code, and reasoning tasks."
                   :request-params (:min_p 0.1))
                  (Qwen2.5-Coder-32B-Instruct
                   :description "Qwen2.5-Coder is the latest series of Code-Specific Qwen large language models (formerly known as CodeQwen)."
                   ;; From https://qwen.readthedocs.io/en/latest/benchmark/speed_benchmark.html
                   :request-params (:min_p 0.1))
                  (Qwen2.5-Coder-14B-Instruct
                   :description "Qwen2.5-Coder is the latest series of Code-Specific Qwen large language models (formerly known as CodeQwen)."
                   ;; From https://qwen.readthedocs.io/en/latest/benchmark/speed_benchmark.html
                   :request-params (:frequency_penalty 0
                                    :min_p 0.1
                                    :presence_penalty 0
                                    :temperature 0.7
                                    :top_k 20
                                    :top_p 0.8
                                    :repetition_penalty 1)))))

(use-package gptel
  :commands (gptel gptel-menu gptel-rewrite gptel-send gptel--ollama-fetch-models)
  :bind (:map gptel-mode-map
              ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
              ("C-c C-c" . #'gptel-abort))
  :hook
  (gptel-mode . (lambda () (olivetti-mode 1)))

  :custom
  ((gptel-default-mode 'org-mode "Use org-mode as the default")
   (gptel-window-select t "Select the window after creation")
   (gptel-window-side 'right "Display on the right side")
   ;; https://github.com/karthink/gptel?tab=readme-ov-file#extra-org-mode-conveniences
   (gptel-org-branching-context t)
   (gptel-expert-commands t)
   (gptel-include-reasoning t))

  :config
  (setq ar-emacs-llm-prompts-dir (expand-file-name "llm/prompts" user-emacs-directory))

  (setq gptel-model 'Qwen3-30B
        gptel-backend ar-emacs-gptel-backend-vllm)

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

  ;; See https://github.com/karthink/gptel/issues/447
  ;; Commit cherry-picked: cbe6f30
  ;; (advice-add 'gptel-menu :before (lambda () (gptel--ollama-fetch-models "Ollama")))

  ;; https://github.com/karthink/gptel?tab=readme-ov-file#extra-org-mode-conveniences
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n"))

(defun ar-emacs-minuet-js-few-shots ()
  "JavaScript few-shots configuration."
  `((:role "user"
    :content ,(string-join
               (list "// language: javascript"
                     "<contextAfterCursor>"
                     ""
                     "fib(5)"
                     "<contextBeforeCursor>"
                     "function fibonacci(n) {"
                     "    <cursorPosition>")
               "\n"))
    (:role "assistant"
     :content ,(string-join
                (list "    // Recursive Fibonacci implementation"
                      "    if (n < 2) {"
                      "        return n;"
                      "    }"
                      "    return fibonacci(n - 1) + fibonacci(n - 2);"
                      "<endCompletion>"
                      "    // Iterative Fibonacci implementation"
                      "    let a = 0, b = 1;"
                      "    for (let i = 0; i < n; i++) {"
                      "        [a, b] = [b, a + b];"
                      "    }"
                      "    return a;"
                      "<endCompletion>"
                      "")
                "\n"))))

(defun ar-emacs-minuet-clojure-few-shots ()
  "Clojure few-shots configuration."
  `((:role "user"
     :content ,(string-join
                (list ";; language: clojure"
                      "<contextAfterCursor>"
                      ""
                      "(fib 5)"
                      "<contextBeforeCursor>"
                      ";; max is which fib number you'd like computed (0th, 1st, 2nd, etc.)"
                      ";; n is which fib number you're on for this call (0th, 1st, 2nd, etc.)"
                      ";; j is the nth fib number (ex. when n = 5, j = 5)"
                      ";; i is the nth - 1 fib number"
                      "(defn- fib-iter"
                      "  \"A simple interative process (using a recursive function) that carries state"
                      "  along with it (as args) until it reaches a solution.\""
                      "  [max n i j]"
                      "  <cursorPosition>")
                "\n"))
    (:role "assistant"
     :content ,(string-join
                (list "  (if (= n max)"
                      "    j"
                      "    (recur max"
                      "           (inc n)"
                      "           j"
                      "           (+ i j))))"
                      "<endCompletion>"
                      "(defn fib"
                      "  [max]"
                      "  (if (< max 2)"
                      "    max"
                      "    (fib-iter max 1 0N 1N)))"
                      "<endCompletion>"
                      "")
                "\n"))))

(defun ar-emacs-minuet-few-shots ()
  "Minuet :few-shots that dependents on the major-mode."
  (cond
   ((derived-mode-p 'js-mode)      (ar-emacs-minuet-js-few-shots))
   ((derived-mode-p 'clojure-mode) (ar-emacs-minuet-clojure-few-shots))
   ((derived-mode-p 'python-mode)  minuet-default-fewshots)
   (t                              minuet-default-fewshots)))

(use-package minuet
  :init (add-to-list 'load-path (expand-file-name "lib/minuet-ai.el" user-emacs-directory))
  :commands (minuet-complete-with-minibuffer minuet-show-suggestion minuet-accept-suggestion)
  :bind (:map minuet-active-mode-map
              ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
              ("<prior>" . #'minuet-previous-suggestion)
              ("<next>" . #'minuet-next-suggestion)
              ("M-S-RET" . #'minuet-accept-suggestion)
              ;; Accept the first line of completion, or N lines with a numeric-prefix:
              ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
              ;; ("M-a" . #'minuet-accept-suggestion-line)
              ("C-g" . #'minuet-dismiss-suggestion)
              )

  :custom
  (minuet-provider 'openai-fim-compatible) ;; 'openai-fim-compatible
  (minuet-n-completions 10)

  ;; I recommend beginning with a small context window size and incrementally
  ;; expanding it, depending on your local computing power. A context window
  ;; of 512, serves as an good starting point to estimate your computing
  ;; power. Once you have a reliable estimate of your local computing power,
  ;; you should adjust the context window to a larger value.
  (minuet-context-window 4096)

  :config
  (setq minuet-openai-compatible-options
   `(:name "Ollama"
     :end-point ,(concat "http://" (ar-emacs-gptel-vllm-endpoint) "/v1/chat/completions")
     :api-key "TERM"
     :model "Qwen/Qwen2.5-Coder-32B-Instruct-AWQ"
     :system (:template minuet-default-system-template
              :prompt minuet-default-prompt
              :guidelines minuet-default-guidelines
              :n-completions-template minuet-default-n-completion-template)
     :fewshots ar-emacs-minuet-few-shots
     :chat-input (:template minuet-default-chat-input-template
                  :language-and-tab minuet--default-chat-input-language-and-tab-function
                  :context-before-cursor minuet--default-chat-input-before-cursor-function
                  :context-after-cursor minuet--default-chat-input-after-cursor-function)))

  (setq minuet-openai-fim-compatible-options
  `(:name "Ollama FIM"
    :end-point ,(concat "http://" (ar-emacs-gptel-vllm-endpoint) "/v1/completions")
    :api-key "TERM"
    :model "Qwen/Qwen2.5-Coder-32B-Instruct-AWQ"
    :template (:prompt minuet--default-fim-prompt-function
               :suffix minuet--default-fim-suffix-function)))

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 512)

  (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 512)
  (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)
  )

;;; llm-conf.el ends here
