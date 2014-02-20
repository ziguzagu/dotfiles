;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cd "~")
(add-to-list 'load-path "~/.emacs.d")
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

;; emacs server/client on tmux
(add-hook 'after-init-hook 'server-start)
(if (getenv "TMUX")
    (shell-command "tmux display -p '#I' > ~/.emacs.d/emacs-server-window"))
(add-hook 'emacs-kill-hook
          (lambda ()
            (shell-command
             "rm ~/.emacs.d/emacs-server-window")))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; el-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; extra recepies
(setq el-get-sources
      '(
        (:name anything-show-completion
               :type http
               :url "http://www.emacswiki.org/cgi-bin/wiki/download/anything-show-completion.el")
        (:name anything-c-moccur
               :type http
               :url "http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el"
               :depends (anything color-moccur))
        (:name dabbrev-highlight
               :type http
               :url "http://www.namazu.org/~tsuchiya/elisp/dabbrev-highlight.el")
        (:name vcl-mode
               :type http
               :url "https://raw.github.com/ssm/elisp/master/vcl-mode.el")
        (:name plenv
               :type github
               :pkgname "karupanerura/plenv.el")
        (:name nginx-mode
               :type github
               :pkgname "ajc/nginx-mode")
        ))

(el-get 'sync)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "init-appearance")
(load "init-minibuffer")
(load "init-scratch")
(load "init-dired")
(load "init-shell")

(load "init-general")
(load "init-complete")
(load "init-anything")
(load "init-migemo")
(load "init-sdic")

(load "init-cpp")
(load "init-perl")
(load "init-html")
(load "init-conf")
