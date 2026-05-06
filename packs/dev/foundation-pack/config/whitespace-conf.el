;;; built-in.el --- Built-in Configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; The whitespace code was originally in other files but was in Emacs 31 started throwing:
;;   Debugger entered--Lisp error: (error "Defining as dynamic an already lexical var" whitespace-style)

;;; Code:

(defvar ar-emacs-ignore-whitespace-modes '(markdown-mode org-mode))

(defun ar-emacs--cleanup-whitespace ()
  "Remove trailing whitespace and blank lines before saving, unless in an ignored mode.
    
This function temporarily overrides `whitespace-style' to only target
'trailing' and 'empty' whitespace for cleanup, preserving your global
visualization settings."
  ;; Only proceed if the current mode is NOT in the ignore list
  (when (not (member major-mode live-ignore-whitespace-modes))
    ;; Use cl-letf to safely change the dynamic value of whitespace-style
    ;; temporarily. This avoids the "Defining as dynamic an already lexical var" error.
    (require 'cl-lib)
    (cl-letf (((symbol-value 'whitespace-style) '(trailing empty)))
      (whitespace-cleanup))))

(use-package whitespace
  :ensure nil
  :config
  ;; Set GLOBAL visualization style
  (setq whitespace-style '(face trailing lines tabs newline space-mark tab-mark newline-mark))

  ;; http://stackoverflow.com/questions/7874548/emacs-23-3-1-whitespace-style
  (setq whitespace-display-mappings
        '((space-mark   ?\     [? ]) ;; use space not dot
          (space-mark   ?\xA0  [?\u00A4]     [?_])
          (space-mark   ?\x8A0 [?\x8A4]      [?_])
          (space-mark   ?\x920 [?\x924]      [?_])
          (space-mark   ?\xE20 [?\xE24]      [?_])
          (space-mark   ?\xF20 [?\xF24]      [?_])
          (newline-mark ?\n    [?$ ?\n])
          (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])))

  ;; (add-hook 'before-save-hook #'ar-emacs--cleanup-whitespace)
  )

(use-package ws-butler
  :diminish
  :commands (ws-butler-mode)
  :hook
  (prog-mode . ws-butler-mode)
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  (ws-butler-trim-predicate (lambda (_ end)
                              (with-demoted-errors
                                  "Debug: Checking face at %d, got %s"
                                (let* ((face-at-end (get-text-property end 'face))
                                       (is-string-face (eq 'font-lock-string-face face-at-end)))
                                  (message "debug: end=%d face=%S is-string-face=%S" end face-at-end is-string-face)
                                  (not is-string-face))))))

;;; whitespace-conf.el ends here
