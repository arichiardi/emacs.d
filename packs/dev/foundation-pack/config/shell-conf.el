;;; shell-conf.el --- Shell Configuration

;;; Commentary:

;;; Code:

(require 'term)

(defcustom eshell-directory-name
  (let* ((dir (concat live-tmp-dir "eshell")))
    (make-directory dir t)
    dir)
  "The directory where Eshell control files should be kept."
  :type 'directory
  :group 'eshell)

;; from http://emacs-journey.blogspot.com.au/2012/06/improving-ansi-term.html
;; kill the buffer when terminal is exited
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "I don't know what this does I just copied it."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defun live-term-use-utf8 ()
  "Set coding system for the current process."
  (set-process-coding-system 'utf-8-unix 'utf-8-unix))
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

;; From https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

(defun ar-emacs--eshell-here ()
  "Open up a new shell in the dir of the current buffer's file.

 The eshell is renamed to match that directory to make multiple
eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-shell.el
(use-package xterm-color
  :defines (compilation-environment
            eshell-preoutput-filter-functions
            eshell-output-filter-functions)
  :init
  ;; For shell and interpreters
  (setenv "TERM" "xterm-256color")
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled
              (make-local-variable 'font-lock-function)
              (setq font-lock-function #'ignore)))

  ;; For eshell
  (with-eval-after-load 'esh-mode
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

  ;; For compilation buffers
  (setq compilation-environment '("TERM=xterm-256color"))

  :bind (("C-c x e" . ar-emacs--eshell-here)))

(use-package eshell-mode
  ;; window-normalize-buffer: No such live buffer *helm-mode-completion-at-point* [2 times]
  ;; helm-M-x-execute-command: No such live buffer *helm pcomplete*
  ;; :hook
  ;; (eshell-mode . (lambda ()
  ;;                  (eshell-cmpl-initialize)))

  :bind
  ;; ([remap eshell-pcomplete] . helm-esh-pcomplete)
  ([remap eshell-list-history] . helm-eshell-history))

(use-package comint
  :bind (:map comint-mode-map
         ("C-r" . comint-history-isearch-backward)
         ("C-M-r" . comint-history-isearch-backward-regexp))
  :custom
  (comint-buffer-maximum-size 20000 "Increase comint buffer size.")
  (comint-prompt-read-only t "Make the prompt read only."))

;;; shell-conf.el ends here
