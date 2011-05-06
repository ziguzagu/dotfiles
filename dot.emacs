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

;;;;;; emacsclient
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

;;;;;; environment
(load "init-appearance")
(load "init-minibuffer")
(load "init-scratch")
(load "init-dired")
(load "init-shell")

;;;;;; generic editor functions
(load "init-general")
(load "init-complete")
(load "init-anything")
(load "init-migemo")

;;;;;; programming lang
(load "init-cpp")
(load "init-perl")
(load "init-html")
(load "init-yaml")

;;;;;; sdic
(when (locate-library "sdic")
  (autoload 'sdic-describe-word
    "sdic" "$B1QC18l$N0UL#$rD4$Y$k(B" t nil)
  (global-set-key "\C-cw" 'sdic-describe-word)
  (autoload 'sdic-describe-word-at-point
    "sdic" "$B%+!<%=%k$N0LCV$N1QC18l$N0UL#$rD4$Y$k(B" t nil)
  (global-set-key "\C-cW" 'sdic-describe-word-at-point)
  ;; $B1QOB8!:w$G;HMQ$9$k<-=q(B
  (setq sdic-eiwa-dictionary-list
        '((sdicf-client "~/opt/dict/eijirou.sdic")))
  ;; $BOB1Q8!:w$G;HMQ$9$k<-=q(B
  (setq sdic-waei-dictionary-list
        '((sdicf-client "~/opt/dict/waeijirou.sdic")))
  ;; $BJ8;z?'(B
  (setq sdic-face-color "brightmagenta")
  )
