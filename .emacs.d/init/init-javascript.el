(use-package js2-mode
  :mode (("\\.js\\'"   . js2-mode)
         ("\\.jsx\\'"  . js2-jsx-mode))
  :interpreter ("node" . js2-mode)
  :custom
  (js2-basic-offset 2))

(use-package json-mode)

(use-package typescript-mode
  :custom
  (typescript-indent-level 2))

(use-package coffee-mode
  :mode "\\.coffee\\'"
  :custom
  (coffee-tab-width 2)
  (coffee-indent-like-python-mode t))

(use-package prettier-js
  :hook
  (js2-mode . prettier-js))