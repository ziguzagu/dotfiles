(use-package lsp-mode
  :commands
  (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; Use terraform-ls instead of terraform-lsp to be stable
  ;; https://github.com/hashicorp/terraform-ls/blob/master/docs/USAGE.md#emacs
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
                    :major-modes '(terraform-mode)
                    :server-id 'terraform-ls)))

(use-package company-lsp
  :pin melpa
  :defer t
  :after lsp company)
