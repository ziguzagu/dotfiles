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
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;;;;; el-get
;; https://github.com/dimitri/el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; install el-get
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))
;; extra recepies
(setq el-get-sources
      '(
        (:name anything-c-moccur
               :type http
               :url "http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el"
               :depends (anything color-moccur))
        (:name dabbrev-highlight
               :type http
               :url "http://www.namazu.org/~tsuchiya/elisp/dabbrev-highlight.el")
        (:name zlc
               :type github
               :pkgname "mooz/emacs-zlc")
        ))
;; download
(el-get 'sync)

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
(load "init-conf")
