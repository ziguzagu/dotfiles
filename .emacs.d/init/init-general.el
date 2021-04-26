;; scroll by line
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; scroll up and down
(global-set-key (kbd "M-p") 'scroll-down)
(global-set-key (kbd "M-n") 'scroll-up)

;; enable upcase/downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; key bind
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c h") 'help-for-help)
(global-set-key (kbd "C-h")  'delete-backward-char)
(global-set-key (kbd "C-c ]") 'align-regexp)
;; C-h as delete in mini buffer
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

(defun my:delete-word-at-point ()
  "Delete the word at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (kill-region (car bounds) (cdr bounds)))))
(global-set-key (kbd "M-d") 'my:delete-word-at-point)

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

;; backup
(defconst my-backup-dir (expand-file-name (format "emacs%d/backup" (user-uid)) temporary-file-directory))
(setq backup-directory-alist `((".*" . ,my-backup-dir))
      auto-save-file-name-transforms `((".*" ,my-backup-dir t))
      auto-save-list-file-prefix my-backup-dir)
(setq backup-by-copying t
      version-control t
      kept-new-versions 5
      kept-old-versions 1
      delete-old-versions t)

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
  (setq ispell-extra-args '("--ignore-case"
                            "--sug-mode=ultra"
                            "--lang=en_US"
                            ;; work for camel case
                            "--run-together"
                            "--run-together-min=2"
                            "--run-together-limit=16"))
  :hook (prog-mode . flyspell-prog-mode))

(use-package wgrep)
