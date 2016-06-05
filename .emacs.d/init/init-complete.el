;; auto-complete
(use-package auto-complete-config
  :diminish auto-complete-mode
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-dict")
  (ac-config-default))

;; dabbrev
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace nil)
(global-set-key "\C-o" 'dabbrev-expand)

;; dabbrev by ac-dabbrev
(require 'ac-dabbrev)
(add-to-list 'ac-sources 'ac-source-dabbrev)
(defun ac-dabbrev-expand ()
  (interactive)
  (auto-complete '(ac-source-dabbrev)))
(global-set-key "\M-o" 'ac-dabbrev-expand)

;; yasnippet
(use-package yasnippet
  :bind ("M-i" . yas/expand)
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))
