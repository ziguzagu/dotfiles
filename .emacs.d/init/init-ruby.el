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
  :pin melpa ;; I need v >= 1.20.0 to use with docker
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
  :bind (:map projectile-rails-mode-map
         ("C-c r m" . projectile-rails-find-current-model)
         ("C-c r M" . projectile-rails-find-model)
         ("C-c r c" . projectile-rails-find-current-controller)
         ("C-c r C" . projectile-rails-find-controller)
         ("C-c r s" . projectile-rails-find-current-spec)
         ("C-c r S" . projectile-rails-find-spec))
  :config
  (projectile-rails-global-mode))

(use-package haml-mode
  :pin melpa)
