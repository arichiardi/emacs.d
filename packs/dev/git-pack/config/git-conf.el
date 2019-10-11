(use-package git-modes
  :defer t)

(use-package browse-at-remote
  :defer t
  :bind (("C-c g b" . browse-at-remote)
         ("C-c g k" . browse-at-remote-kill)))
