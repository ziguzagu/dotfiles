(setq initial-scratch-message nil)

(use-package unkillable-scratch
  :config (unkillable-scratch t))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))
