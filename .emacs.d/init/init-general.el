;; tab/indent
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; scroll by line
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; scroll up and down
(global-set-key (kbd "M-p") 'scroll-down)
(global-set-key (kbd "M-n") 'scroll-up)

;; comment style
(setq comment-style 'multi-line)

;; enable upcase/downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; key bind
(global-set-key "\r" 'newline-and-indent)
(global-set-key "\C-ch" 'help-for-help)
(global-set-key "\C-h"  'delete-backward-char)
(global-set-key "\C-c]" 'align-regexp)
;; C-h as delete in mini buffer
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; save recentf a lot for helm
(require 'recentf)
(setq recentf-max-saved-items 5000)

;; reload a file when it was changed by another process (include vc)
(global-auto-revert-mode t)
(setq auto-revert-check-vc-info t)

;; cycle buffer
(defun my:switch-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") 'my:switch-last-buffer)

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
(defun my:window-toggle-split ()
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
(global-set-key (kbd "C-x 9") 'my:window-toggle-split)

;; split window or move other window by one keybind
(defun my:other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-z") 'my:other-window-or-split)

;; popwin
(use-package popwin
  :custom
  (popwin:popup-window-position 'bottom)
  (popwin:popup-window-height 20)
  :config
  (popwin-mode 1))

;; reload buffer without confirmation
(defun my:revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))
(global-set-key (kbd "M-r") 'my:revert-buffer-no-confirm)

;; send contents cut and copied to clipboard
(when (eq system-type 'darwin)
  (defun my:copy-from-osx ()
    "Get clipboard contents."
    (shell-command-to-string "pbpaste"))

  (defun my:paste-to-osx (text &optional push)
    "Paste yanked contents to clipboard."
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'my:paste-to-osx)
  (setq interprogram-paste-function 'my:copy-from-osx))

(use-package expand-region
  :bind (("C-]" . er/expand-region)
         ("M-]" . er/contract-region)))

(use-package multiple-cursors
  :bind (("C-M-c" . mc/edit-lines)
         ("C-M-n" . mc/mark-next-like-this)
         ("C-M-p" . mc/mark-previous-like-this)
         ("C-M-a" . mc/mark-all-like-this)))

(use-package flyspell
  :init
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra"
                            "--lang=en_US"
                            ;; work for camel case
                            "--run-together"
                            "--run-together-min=2"
                            "--run-together-limit=16"))
  :hook (prog-mode . flyspell-prog-mode))
