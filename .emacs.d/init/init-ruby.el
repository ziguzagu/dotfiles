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
    ;; complete end automatically
    (require 'ruby-end)
    ;; highlight begining of block cursor on the end
    (require 'ruby-block)
    (ruby-block-mode t)
    (setq ruby-block-highlight-toggle t)
    ;; enable projectile
    (projectile-mode))

  (add-hook 'ruby-mode-hook 'my-ruby-mode))

;;; init-ruby.el ends here
