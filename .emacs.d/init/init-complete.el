;;; init-complete.el --- Configure completion
;;; Commentary:
;;; Code:

;; auto-complete
(use-package auto-complete-config
  :diminish auto-complete-mode
  :init
  (custom-set-faces
   '(popup-face ((t (:background "gray28" :foreground "gray72")))))
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-dict")
  (ac-config-default))

;; dabbrev
;; (setq dabbrev-case-fold-search t)
;; (setq dabbrev-case-replace nil)
;; (global-set-key "\C-o" 'dabbrev-expand)

(use-package ac-dabbrev
  :init
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  :config
  (add-to-list 'ac-sources 'ac-source-dabbrev)
  (defun ac-dabbrev-expand ()
    (interactive)
    (auto-complete '(ac-source-dabbrev)))
  (global-set-key "\C-o" 'ac-dabbrev-expand))

;; yasnippet
(use-package yasnippet
  :bind ("M-i" . yas/expand)
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

;;; init-complete.el ends here
