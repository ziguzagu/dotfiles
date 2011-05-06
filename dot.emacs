;;;; -*- mode: lisp-interaction; syntax: elisp; coding: iso-2022-7bit -*-

;;;;;; Basics
(cd "~")
(setq user-full-name "Hiroshi Sakai")
(setq user-mail-address "ziguzagu@gmail.com")
(setq load-path
      (append (list "~/.emacs.d"
                    "~/.emacs.d/lisp") load-path))
;; using unsafe local variables..?
(setq safe-local-variable-values (quote ((syntax . elisp))))

;;;;;; Languages
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)


;;;;;; EmacsClient
(add-hook 'after-init-hook 'server-start)
(shell-command "echo $WINDOW >~/.emacs.d/emacs-server-window")
(add-hook 'emacs-kill-hook
          (lambda ()
            (shell-command
             "rm ~/.emacs.d/emacs-server-window")))
(add-hook 'server-done-hook
          (lambda ()
            (shell-command
             "screen -X select `cat ~/.emacs.d/emacsclient-caller`")))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


(load "init-appearance")
(load "init-minibuffer")

;; init editor
(load "init-general")
(load "init-complete")

;;;;;; anything
;;  * http://www.emacswiki.org/emacs/Anything
(require 'anything-config)
;; call show-kill-ring function by hand
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
;; minimal anything
(defun my-anything ()
  (interactive)
  (anything-other-buffer '(anything-c-source-buffers+
                           anything-c-source-recentf
                           anything-c-source-files-in-current-dir+)
                         "*my anything*"))
(global-set-key (kbd "C-c ;") 'my-anything)
;; woman
(defun my-anything-woman ()
  (interactive)
  (anything-other-buffer '(anything-c-source-man-pages)
                         "*my woman*"))
(global-set-key (kbd "C-c m") 'my-anything-woman)
;; color-moccur + anything
(require 'color-moccur)
(setq moccur-split-word t)
(require 'anything-c-moccur)
(setq anything-c-moccur-enable-auto-look-flag t)
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur)


;;;;;; Misc
;; move divided windows by shift with cursor.
(windmove-default-keybindings)
;; ;; woman
;; (global-set-key "\C-cm" 'woman)


;;;;;; window
;; vertical <-> horizontal
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


;;;;;; Dired
(add-hook 'dired-load-hook
          '(lambda ()
                  ;; show directories at the top of buffer
                  (setq ls-lisp-dirs-first t)
                  ;; ls options
                  (setq dired-listing-switches "-AlhFG")
                  ;; recursive copy/delete
                  (setq dired-recursive-copies 'always)
                  (setq dired-recursive-deletes 'always)))
;; don't create new buffer at moving direcotry
(put 'dired-find-alternate-file 'disabled nil)
;; a lump-sum rename
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
;; don't create new buffer when move to subdirectory
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;; uniqufy
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")
;; using ffap
(ffap-bindings)
(setq read-file-name-completion-ignore-case t)


;;;;;; shell
;; completation on shell-mode
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")
;; hide password
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)
;; handle escape sequence
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;;;;;; generic config file
(require 'generic-x)
(setq auto-mode-alist
      (append '(("\\.conf$" . default-generic-mode)
                ("\\.cfg$"  . default-generic-mode)) auto-mode-alist))


;;;;;; programming lang
(load "init-cpp")
(load "init-perl")
(load "init-html")


;;;;;; migemo
(when (locate-library "migemo")
  (setq migemo-directory "/usr/share/migemo")
  (load "migemo")
  (migemo-init)
  ;; cache
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)
  ;; delay for accepting STDOUT
  (setq migemo-accept-process-output-timeout-msec 80)
  ;; turn off migemo wheren copy strings from buffer
  (defadvice isearch-yank-string
    (before migemo-off activate)
    (setq migemo-isearch-enable-p nil))
  ;; turn on migemo when searching by isearch
  (defadvice isearch-mode
    (before migemo-on activate)
    (setq migemo-isearch-enable-p t))
  )

;;;;;; *scratch*
;; save and restore the content of *scratch* buffer
(defun save-scratch-data ()
  (let ((str (progn
               (set-buffer (get-buffer "*scratch*"))
               (buffer-substring-no-properties
                (point-min) (point-max))))
        (file "~/.emacs.d/scratch"))
    (if (get-file-buffer (expand-file-name file))
        (setq buf (get-file-buffer (expand-file-name file)))
      (setq buf (find-file-noselect file)))
    (set-buffer buf)
    (erase-buffer)
    (insert str)
    (save-buffer)))

(defadvice save-buffers-kill-emacs
  (before save-scratch-buffer activate)
  (save-scratch-data))

(defun read-scratch-data ()
  (let ((file "~/.emacs.d/scratch"))
    (when (file-exists-p file)
      (set-buffer (get-buffer "*scratch*"))
      (erase-buffer)
      (insert-file-contents file))
    ))

(read-scratch-data)

;; make not to kill *scratch* buffer
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(defun my-buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))

;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
(add-hook 'kill-buffer-query-functions
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my-make-scratch 0) nil)
                        t))))

;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
(add-hook 'after-save-hook
          (function (lambda ()
                      (unless (member "*scratch*" (my-buffer-name-list))
                        (my-make-scratch 1)))))


;;;;;; yaml-mode
(autoload 'yaml-mode "yaml-mode" nil t)
(setq auto-mode-alist
      (append '(("\\.yml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)) auto-mode-alist))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;;;;;; sdic
(when (locate-library "sdic")
  (autoload 'sdic-describe-word
    "sdic" "英単語の意味を調べる" t nil)
  (global-set-key "\C-cw" 'sdic-describe-word)
  (autoload 'sdic-describe-word-at-point
    "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
  (global-set-key "\C-cW" 'sdic-describe-word-at-point)
  ;; 英和検索で使用する辞書
  (setq sdic-eiwa-dictionary-list
        '((sdicf-client "~/opt/dict/eijirou.sdic")))
  ;; 和英検索で使用する辞書
  (setq sdic-waei-dictionary-list
        '((sdicf-client "~/opt/dict/waeijirou.sdic")))
  ;; 文字色
  (setq sdic-face-color "brightmagenta")
  )
