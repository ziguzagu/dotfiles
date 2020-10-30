;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :hook
  (go-mode . lsp-deferred)
  (go-mode . lsp-go-install-save-hooks))
