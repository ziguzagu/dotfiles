(use-package vertico
  :pin melpa-stable
  :config
  (vertico-mode))

(use-package consult
  :pin melpa-stable
  :bind (("C-c s" . consult-imenu)))

(use-package orderless
  :pin melpa-stable
  :custom
  (completion-styles '(orderless)))
