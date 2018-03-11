(live-add-pack-lib "geiser/elisp")

(require 'geiser)

(add-hook 'geiser-mode-hook
          (lambda ()
            (enable-paredit-mode)
            (flyspell-prog-mode)
            (linum-mode t)
            (rainbow-delimiters-mode-enable)
            (git-gutter-mode t)
            (company-mode-on)))
