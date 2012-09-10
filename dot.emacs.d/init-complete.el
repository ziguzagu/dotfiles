;; auto-complete
(el-get 'sync '(popup auto-complete))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-dict")
(ac-config-default)

;; dabbrev
(require 'dabbrev-highlight)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace nil)
(global-set-key "\C-o" 'dabbrev-expand)

;; yasnippet
(el-get 'sync 'yasnippet)
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
