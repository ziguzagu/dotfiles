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
(load "init-scratch")
(load "init-dired")

(load "init-general")
(load "init-complete")
(load "init-anything")


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
