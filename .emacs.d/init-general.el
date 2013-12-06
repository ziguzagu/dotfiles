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
(global-set-key "\C-h"  'delete-backward-char)
(global-set-key "\C-c]" 'align-regexp)

;; reload a file when it was changed by another process (include vc)
(global-auto-revert-mode t)
(setq auto-revert-check-vc-info t)

;; cycle buffer
(defun switch-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") 'switch-last-buffer)

;; vc
(require 'vc-git)
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

;; move divided windows by shift with cursor.
(windmove-default-keybindings)

;; rotate window divide vertical / horizontal
(defun window-toggle-split ()
  "toggle splitted windows vertical and horizontal"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "no splitted windows"))
  (let (before-height (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally))
    (switch-to-buffer-other-window other-buf)
    (other-window -1)))
(global-set-key "\C-x9" 'window-toggle-split)

;; popwin
(el-get 'sync 'popwin)
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(push '("*vc-diff*" :height 20) popwin:special-display-config)
(push '("*vc-change-log*" :height 20) popwin:special-display-config)

;; markdown
(el-get 'sync 'markdown-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(el-get 'sync 'org-mode)
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))
;; org-capture
(setq org-capture-templates
      '(("m" "Memo" entry (file+datetree (concat org-directory "memo.org"))
         "* %?\n")
        ("a" "Annotation" entry (file (concat org-directory "annon.org"))
         "* %?\n %U\n %i\n %a\n")
        ("i" "Idea" entry (file (concat org-directory "ideas.org"))
         "* %?\n %U\n")
        ))
(global-set-key (kbd "C-c c") 'org-capture)
;; hilight lines in code block
(setq org-src-fontify-natively t)
