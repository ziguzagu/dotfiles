;;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8 -*-

;;;;;; basics
(cd "~")
(setq load-path
      (append (list "~/.emacs.d"
                    "~/.emacs.d/lisp") load-path))
;; using unsafe local variables..?
(setq safe-local-variable-values (quote ((syntax . elisp))))

;;;;;; profile
(setq user-full-name "Hiroshi Sakai")
(setq user-mail-address "ziguzagu@gmail.com")

;;;;;; languages
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)

;;;;;; emacsclient on screen
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
(load "init-sdic")

;;;;;; programming lang
(load "init-cpp")
(load "init-perl")
(load "init-html")
(load "init-yaml")

;; testng vcl mode
(autoload 'vcl-mode "vcl-mode" nil t)
(setq auto-mode-alist
      (append '(("\\.vcl$" . vcl-mode)) auto-mode-alist))
(add-hook 'vcl-mode-hook
          (lambda ()
            (setq vcl-indent-level 4)))

;; testing howm
(add-to-list 'load-path "~/.emacs.d/lisp/howm")
(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(autoload 'howm-menu "howm" nil t)
(setq howm-directory "~/dev/howm")
