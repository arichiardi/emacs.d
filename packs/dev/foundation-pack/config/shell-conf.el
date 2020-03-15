(require 'term)

(defcustom eshell-directory-name
  (let* ((dir (concat live-tmp-dir "eshell")))
    (make-directory dir t)
    dir)
  "The directory where Eshell control files should be kept."
  :type 'directory
  :group 'eshell)

;; kill buffer when terminal process is killed
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defun live-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'live-term-use-utf8)

(defun live-term-paste (&optional string)
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0))))

(defun live-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'live-term-paste))

(add-hook 'term-mode-hook 'live-term-hook)

;; From https://github.com/atomontage/xterm-color

(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; Disable font-locking in this buffer to improve performance
            (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            (make-local-variable 'font-lock-function)
            (setq font-lock-function (lambda (_) nil))
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

;; Also set TERM accordingly (xterm-256color) in the shell itself.

;; You can also use it with eshell (and thus get color output from system ls):


;; Fix (void eshell-preoutput-filter-functions)
;; from https://github.com/atomontage/xterm-color/issues/4
(with-eval-after-load 'esh-mode
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color"))
