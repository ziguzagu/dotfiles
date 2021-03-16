(use-package ruby-mode
  :interpreter "ruby"
  :hook
  (ruby-mode . lsp-deferred)
  :custom
  (ruby-insert-encoding-magic-comment nil))

(use-package ruby-end
  :hook
  (ruby-mode . ruby-end-mode))

(use-package rspec-mode
  :custom
  (rspec-use-docker-when-possible t)
  (rspec-use-relative-path t)
  :config
  (rspec-install-snippets))

(use-package inf-ruby
  :hook
  ((ruby-mode . inf-ruby-minor-mode)
   (ruby-mode . inf-ruby-switch-setup)))

(use-package rbenv
  :config
  (global-rbenv-mode))

(use-package projectile-rails
  :bind-keymap
  ("C-c r" . projectile-rails-command-map)
  :config
  (projectile-rails-global-mode))

(use-package haml-mode)
