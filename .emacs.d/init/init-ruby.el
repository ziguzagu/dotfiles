(use-package ruby-mode
  :mode ("\\.rb\\'" "Capfile" "Gemfile")
  :interpreter "ruby"
  :config
  (defun my-ruby-mode ()
    (custom-set-variables
     '(ruby-insert-encoding-magic-comment nil))
    ;; use flycheck with rubocop
    (setq flycheck-checker 'ruby-rubocop)
    (flycheck-mode t))

  (add-hook 'ruby-mode-hook 'my-ruby-mode))

(use-package ruby-end
  :diminish ruby-end-mode
  :init
  (add-hook 'ruby-mode-hook '(lambda () (ruby-end-mode t))))

(use-package rbenv
  :config
  (global-rbenv-mode))

(use-package projectile-rails
  :diminish projectile-rails-mode
  :config
  (projectile-rails-global-mode))

;; It's hard to working rails projects that uses docker
;; (use-package robe
;;   :init
;;   (add-hook 'ruby-mode-hook '(lambda () (robe-mode)))
;;   (add-hook 'robe-mode-hook '(lambda () (ac-robe-setup))))
