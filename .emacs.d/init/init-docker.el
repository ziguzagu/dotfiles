(use-package dockerfile-mode)
(use-package docker-compose-mode)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
