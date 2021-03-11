(use-package yaml-mode
  :bind (:map yaml-mode-map
         ("C-m" . newline-and-indent)))

(use-package docker-compose-mode)

(use-package dockerfile-mode)

(use-package docker
  :bind ("C-c d" . docker))
