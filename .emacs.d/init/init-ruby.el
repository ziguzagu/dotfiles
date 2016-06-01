;;; init-ruby.el --- customize ruby mode
;;; Commentary:
;;; Code:

(use-package ruby-mode
  :mode ("\\.rb$" "Capfile" "Gemfile")
  :interpreter "ruby"
  :config
  (defun my-ruby-mode ()
    (custom-set-variables
     '(ruby-insert-encoding-magic-comment nil))
    ;; use flycheck with rubocop
    (setq flycheck-checker 'ruby-rubocop)
    (flycheck-mode 1)
    ;; enable projectile
    (projectile-mode))

  (add-hook 'ruby-mode-hook 'my-ruby-mode))

(use-package ruby-end
  :defer t
  :diminish ruby-end-mode
  :init
  (add-hook 'ruby-mode-hook '(lambda () (ruby-end-mode t))))

(use-package ruby-block
  :defer t
  :commands ruby-block-mode
  :diminish ruby-block-mode
  :init
  (add-hook 'ruby-mode-hook '(lambda () (ruby-block-mode t)))
  :config
  (setq ruby-block-highlight-toggle t))

;;; init-ruby.el ends here
