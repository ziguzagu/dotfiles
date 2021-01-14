;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(defun my:insert-short-var-declaration-op ()
  "Insert `:=` at the point"
  (interactive)
  (unless (string= (string (char-before)) " ")
    (insert " "))
  (insert ":=")
  (unless (string= (string (char-after)) " ")
    (insert " ")))

(use-package go-mode
  :bind
  ;; map `C-=` to default ascii codes `^[[61;5u` by iTerm's key map
  (("C-=" . my:insert-short-var-declaration-op))
  :hook
  (go-mode . lsp-deferred)
  (go-mode . lsp-go-install-save-hooks))
