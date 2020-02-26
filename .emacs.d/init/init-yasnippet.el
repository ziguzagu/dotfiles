(use-package yasnippet
  :bind (("C-c i" . yas-expand))
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))
