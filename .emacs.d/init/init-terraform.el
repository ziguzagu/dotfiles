(use-package hcl-mode)

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package company-terraform
  :config
  (company-terraform-init))
