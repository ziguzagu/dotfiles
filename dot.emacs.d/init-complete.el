;; auto-complete
(el-get 'sync '(popup auto-complete))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-dict")
(ac-config-default)

;; dabbrev
(el-get 'sync 'dabbrev-highlight)
(require 'dabbrev-highlight)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace nil)
(global-set-key "\C-o" 'dabbrev-expand)

;; yasnippet
(el-get 'sync 'yasnippet)
(setq yas-trigger-key "M-i")
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"
                         "~/.emacs.d/el-get/yasnippet/snippets"))
(yas-global-mode 1)

;; ;; zlc
;; (el-get 'sync 'zlc)
;; (require 'zlc)
;; (let ((map minibuffer-local-map))
;;   (define-key map (kbd "<backtab>") 'zlc-select-previous)
;;   (define-key map (kbd "S-<tab>") 'zlc-select-previous)
;;   (define-key map (kbd "C-p") 'zlc-select-previous-vertical)
;;   (define-key map (kbd "C-n") 'zlc-select-next-vertical)
;;   (define-key map (kbd "C-b") 'zlc-select-previous)
;;   (define-key map (kbd "C-f") 'zlc-select-next)
;;   )
