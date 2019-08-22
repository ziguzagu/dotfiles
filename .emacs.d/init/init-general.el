;; use command key as meta key in cocoa emacs
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

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
;; C-h as delete in mini buffer
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; save recentf a lot for helm
(setq recentf-max-saved-items 5000)

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

;; split window or move other window by one keybind
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-z") 'other-window-or-split)

;; popwin
(use-package popwin
  :config
  (popwin-mode 1)
  (custom-set-variables '(popwin:popup-window-position 'bottom)
                        '(popwin:popup-window-height 20)))

;; reload buffer without confirmation
(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))
(global-set-key "\M-r" 'revert-buffer-no-confirm)

;; send contents cut and copied to clipboard
(when (eq system-type 'darwin)
  (defun copy-from-osx ()
    "Get clipboard contents."
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    "Paste yanked contents to clipboard."
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'org-mode)
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-startup-truncated nil)
(setq org-startup-folded nil)
(setq org-return-follows-link t)
;; org-capture
(setq org-capture-templates
      '(("m" "Memo" entry (file+datetree (concat org-directory "memo.org"))
         "* [%<%H:%M>] %?\n")))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c x") '(lambda () (interactive) (org-capture nil "m")))
;; hilight lines in code block
(setq org-src-fontify-natively t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expand-region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :bind (("C-]" . er/expand-region)
         ("M-]" . er/contract-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :bind (("C-M-c" . mc/edit-lines)
         ("C-M-n" . mc/mark-next-like-this)
         ("C-M-p" . mc/mark-previous-like-this)
         ("C-M-a" . mc/mark-all-like-this)))
