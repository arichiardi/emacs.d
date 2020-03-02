;;; ocaml-conf.el --- Ocaml Config

;;; Commentary:

;;; Code:

;; from spacemacs
(defun ar-emacs--utop-eval-phrase-and-go ()
  "Evaluate the phrase to REPL and switch to it.

It switches to the REPL in `insert state'."
  (interactive)
  (utop-eval-phrase)
  (utop)
  ;; (evil-insert-state)
  )

(defun ar-emacs--utop-eval-buffer-and-go ()
  "Evaluate the buffer and switch to REPL.

It switches to the REPL in `insert state'."
  (interactive)
  (utop-eval-buffer)
  (utop)
  ;; (evil-insert-state)
  )

(defun ar-emacs--utop-eval-region-and-go (start end)
  "Evaluate region from START to END to REPL then switch to it.

It switches to the REPL in `insert state'."
  (interactive "r")
  (utop-eval-region start end)
  (utop)
  ;; (evil-insert-state)
  )

(defun ar-emacs--configure-ocaml ()
  "Configure OCaml."
  (require 'smartparens-ml)
  (smartparens-strict-mode))

(use-package tuareg-mode
  :defer t
  :mode ("\\.ml[ily]?$" "\\.topml$" "\\.eliomi?\\'")
  :hook ((tuareg-mode . subword-mode)
         (tuareg-mode . rainbow-delimiters-mode-enable)
         (tuareg-mode . company-mode-on)
         (tuareg-mode . ar-emacs--configure-ocaml)))

(use-package merlin
  :defer t
  :config
  ;; (setq merlin-command (shell-cmd "which ocamlmerlin"))
  (setq merlin-completion-with-doc t)
  (add-to-list 'company-backends 'merlin-company-backend)

  :hook
  (tuareg-mode . merlin-mode)
  (caml-mode-hook . merlin-mode)

  ;; Make company aware of merlin
  ;; (with-eval-after-load 'company
    ;; (add-hook 'merlin-mode-hook 'company-mode)
    ;; )

  :bind (:map merlin-mode-map
	     ("M-." . merlin-locate)
	     ("M-," . merlin-pop-stack)
	     ("M-m" . merlin-error-next)
	     ("M-n" . merlin-error-prev)
	     ("C-c C-o" . merlin-occurrences)
	     ("C-c C-j" . merlin-jump)
	     ("C-c i" . merlin-locate-ident)
	     ("C-c C-e" . merlin-iedit-occurrences)))

(use-package utop
  :defer t
  :init
  :hook (tuareg-mode . utop-minor-mode)
  :config
  (setq utop-command "opam config exec -- utop -emacs")
  :bind (("C-c M-j" . utop)
         :map utop-minor-mode-map
         ("C-c C-c" . ar-emacs--utop-eval-phrase-and-go)
         ("C-c C-r" . ar-emacs--utop-eval-region-and-go)
         ("C-c C-b" . ar-emacs--utop-eval-buffer-and-go)
         :map utop-mode-map
         ("C-c C-c" . ar-emacs--utop-eval-phrase-and-go)
         ("C-c C-r" . ar-emacs--utop-eval-region-and-go)
         ("C-c C-b" . ar-emacs--utop-eval-buffer-and-go)
         ("C-<up>" . utop-history-goto-prev)
         ("C-<down>" . utop-history-goto-next)))

;;; ocaml-conf.el ends here
