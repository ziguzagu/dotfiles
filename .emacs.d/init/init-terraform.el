(use-package hcl-mode)

(use-package terraform-mode
  :hook
  ((terraform-mode . terraform-format-on-save-mode)
   (terraform-mode . lsp-deferred)))
