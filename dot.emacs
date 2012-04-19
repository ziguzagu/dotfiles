;;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8 -*-

;;;;;; basics
(cd "~")
(setq load-path
      (append (list "~/.emacs.d"
                    "~/.emacs.d/elisp") load-path))
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

;;;;;; emacs server/client on tmux/screen
(add-hook 'after-init-hook 'server-start)
(if (getenv "TMUX")
    (shell-command "tmux display -p '#I' > ~/.emacs.d/emacs-server-window")
    (shell-command "echo $WINDOW > ~/.emacs.d/emacs-server-window"))
(add-hook 'emacs-kill-hook
          (lambda ()
            (shell-command
             "rm ~/.emacs.d/emacs-server-window")))
(if (getenv "TMUX")
    (add-hook 'server-done-hook
              (lambda ()
                (shell-command
                 "tmux select-window -t `cat ~/.emacs.d/emacs-client-window`")))
    (add-hook 'server-done-hook
              (lambda ()
                (shell-command
                 "screen -X select `cat ~/.emacs.d/emacs-client-window`"))))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;;;;; auto-install
(require 'auto-install)
(add-to-list 'load-path "~/.emacs.d/auto-install")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

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
(autoload 'vcl-mode "vcl-mode" "Major mode for editting varnish config file")
(add-to-list 'auto-mode-alist '("\\.vcl$" . vcl-mode))
(add-hook 'vcl-mode-hook
          (lambda ()
            (setq vcl-indent-level 4)))

;; testing puppet mode
(auto-install-from-url "https://raw.github.com/puppetlabs/puppet-syntax-emacs/master/puppet-mode.el")
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
