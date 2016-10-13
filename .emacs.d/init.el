;;; init.el --- initialize emacs
;;; Commentary:
;;; Code:

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

;; initialize packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-boring-file-regexp-list (quote ("~$" "\\.elc$" "^#" "/\\.$" "/\\.\\.$")))
 '(helm-buffer-max-length 35)
 '(helm-delete-minibuffer-contents-from-point t)
 '(helm-ff-skip-boring-files t)
 '(helm-ls-git-show-abs-or-relative (quote relative))
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-ls-git helm-source-buffer-not-found)))
 '(helm-truncate-lines t t)
 '(package-selected-packages
   (quote
    (flycheck projectile helm yasnippet yaml-mode web-mode vcl-mode use-package scss-mode rust-mode ruby-end ruby-block robe rbenv puppet-mode popwin plenv nginx-mode multiple-cursors markdown-mode js2-mode helm-projectile helm-ls-git helm-ag go-mode flycheck-pos-tip expand-region exec-path-from-shell elixir-mode dockerfile-mode color-moccur coffee-mode auto-complete ac-dabbrev))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:background "darkolivegreen3" :foreground "black"))))
 '(diff-changed ((t (:background "yellow" :foreground "black"))))
 '(diff-context ((t (:inherit shadow :foreground "gray90"))))
 '(diff-file-header ((t (:background "gray32" :foreground "orange"))))
 '(diff-header ((t (:background "gray32" :foreground "gray70"))))
 '(diff-removed ((t (:background "tomato" :foreground "black"))))
 '(helm-candidate-number ((t (:foreground "#00afff"))))
 '(helm-header ((t (:background "#3a3a3a" :underline nil))))
 '(helm-match ((t (:foreground "darkolivegreen3"))))
 '(helm-selection ((t (:background "#005f87" :weight normal))))
 '(helm-source-header ((t (:background "gray16" :foreground "gray64" :slant italic))))
 '(mode-line ((t (:foreground "#bcbcbc" :background "#444444" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#ff8700" :weight normal))))
 '(mode-line-inactive ((t (:foreground "gray42" :background "gray16" :box nil))))
 '(mode-line-vc-mode ((t (:foreground "#5fafff" :weight normal))) t)
 '(popup-face ((t (:background "gray28" :foreground "gray72"))))
 '(whitespace-tab ((t (:foreground "gray70" :background nil :inverse-video nil))))
 '(whitespace-trailing ((t (:foreground "gray90" :background "gray32" :inverse-video nil)))))
