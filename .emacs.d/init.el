;;; init.el --- initialize emacs
;;; Commentary:
;;; Code:

(setq debug-on-error t)
(cd "~")
(add-to-list 'load-path "~/.emacs.d/init")

;; using unsafe local variables..?
(setq safe-local-variable-values (quote ((syntax . elisp))))

;; profile
(setq user-full-name "Hiroshi Sakai")
(setq user-mail-address "ziguzagu@gmail.com")

;; languages
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)

;; custom is junk
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; change frame size to good feeling
(when window-system
  (add-to-list 'default-frame-alist '(width . 140))
  (add-to-list 'default-frame-alist '(fullscreen . fullheight)))

;; XXX: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
;; workaround for Emacs 26.2. It will be fixed 26.3 and then remove it.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; initialize package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; prepare to use use-package.el
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))
(require 'diminish)
(require 'bind-key)

;; emacs server/client on tmux
(with-eval-after-load 'server
  (unless (server-running-p)
    (server-start)
    (cond ((getenv "TMUX")
           (shell-command "tmux display -p '#I' > ~/.emacs.d/emacs-server-window")
           (add-hook 'kill-emacs-hook
                     (lambda ()
                       (shell-command "rm -f ~/.emacs.d/emacs-server-window")))))
    (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)))

;; inherit PATH from shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; initialize
(load "init-appearance")
(load "init-modeline")
;; (load "init-minibuffer")
(load "init-scratch")
(load "init-dired")
(load "init-shell")

(load "init-general")
(load "init-yasnippet")
(load "init-company")
(load "init-helm")
;; (load "init-ivy")
;; (load "init-migemo")
;; (load "init-sdic")

(load "init-coding")
(load "init-cpp")
(load "init-ruby")
(load "init-perl")
(load "init-web")
(load "init-conf")
(load "init-docker")

;;; init.el ends here
