;; tab/indent
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; scroll by line
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; scroll by line with holding cursor.
(defun scroll-up-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))
(defun scroll-down-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))
(global-set-key "\M-p" 'scroll-up-in-place)
(global-set-key "\M-n" 'scroll-down-in-place)

;; comment style
(setq comment-style 'multi-line)

;; enable upcase/downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; key bind
(global-set-key "\r" 'newline-and-indent)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-ch" 'help-for-help)
(global-set-key "\C-h" 'delete-backward-char)

;; re-load a file when it was changed by another process
(global-auto-revert-mode t)

;; vc
(setq vc-follow-symlinks t)
(setq vc-make-backup-files t)

;; autosave and backup
(setq auto-save-default t)
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/bakcup"))
            backup-directory-alist))
(setq backup-by-copying t)
(setq version-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq delete-old-versions t)
