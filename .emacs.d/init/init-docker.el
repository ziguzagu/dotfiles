(use-package yaml-mode
  :bind (:map yaml-mode-map
         ("C-m" . newline-and-indent)))

(use-package docker-compose-mode)

(use-package dockerfile-mode)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
