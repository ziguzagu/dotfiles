;;; init.el --- initialize emacs
;;; Commentary:
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq debug-on-error t)
(cd "~")
(add-to-list 'load-path "~/.emacs.d/init")
(add-to-list 'load-path "~/.emacs.d/elisp")

;; using unsafe local variables..?
(setq safe-local-variable-values (quote ((syntax . elisp))))

;; profile
(setq user-full-name "Hiroshi Sakai")
(setq user-mail-address "ziguzagu@gmail.com")

;; languages
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)

;; prepare to setup packages gracefully
;; (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
;; (cask-initialize)
(load "init-packages")
(require 'use-package)

;; emacs server/client on tmux
(use-package server
  :config
  (unless (server-running-p)
    (server-start)
    (cond ((getenv "TMUX")
           (shell-command "tmux display -p '#I' > ~/.emacs.d/emacs-server-window")
           (add-hook 'kill-emacs-hook
                     (lambda ()
                       (shell-command "rm -f ~/.emacs.d/emacs-server-window")))))
    (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)))

;; inherit PATH from shell
(exec-path-from-shell-initialize)

;; initialize
(load "init-appearance")
;; (load "init-minibuffer")
(load "init-scratch")
(load "init-dired")
(load "init-shell")

(load "init-general")
(load "init-complete")
(load "init-helm")
;; (load "init-migemo")
;; (load "init-sdic")

(load "init-coding")
(load "init-cpp")
(load "init-ruby")
(load "init-perl")
(load "init-web")
(load "init-conf")
(load "init-elixir")

;;; init.el ends here
