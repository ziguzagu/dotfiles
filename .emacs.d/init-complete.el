;; auto-complete
(require 'popup)
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-dict")
(ac-config-default)

;; replace dabbrev by ac-dabbrev
(require 'ac-dabbrev)
(add-to-list 'ac-sources 'ac-source-dabbrev)
(defun ac-dabbrev-expand ()
  (interactive)
  (auto-complete '(ac-source-dabbrev)))
(global-set-key "\C-o" 'ac-dabbrev-expand)

;; yasnippet
(setq yas-trigger-key "M-i")
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"
                         "~/.emacs.d/el-get/yasnippet/snippets"))
(yas-global-mode 1)
