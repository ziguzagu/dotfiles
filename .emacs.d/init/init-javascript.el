(use-package js2-mode
  :mode (("\\.js\\'"   . js2-mode)
         ("\\.jsx\\'"  . js2-jsx-mode))
  :interpreter ("node" . js2-mode))

(use-package json-mode)

(use-package typescript-mode
  :custom
  (typescript-indent-level 2))

(use-package coffee-mode
  :mode "\\.coffee\\'"
  :custom
  (coffee-tab-width 2)
  (coffee-indent-like-python-mode t))
