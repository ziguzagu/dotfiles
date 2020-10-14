(setq debug-on-error t)
(cd "~")

;; quiet startup
(setq inhibit-startup-message t)

;; preseve  my init.el with saving custom variables to another file
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; initialize package.el
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities '(("melpa" . 10)
                                   ("melpa-stable" . 5)
                                   ("gnu" . 1)))
(package-initialize)

;; prepare to use use-package.el
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))
(require 'bind-key)

;; inherit PATH from shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; emacs server/client on tmux
(require 'server)
(unless (server-running-p)
  (server-start)
  (cond ((getenv "TMUX")
         (shell-command "tmux display -p '#I' > ~/.emacs.d/emacs-server-window")
         (add-hook 'kill-emacs-hook
                   (lambda ()
                     (shell-command "rm -f ~/.emacs.d/emacs-server-window")))))
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

;; initialize
(add-to-list 'load-path "~/.emacs.d/init")
(load "init-appearance")
(load "init-modeline")
(load "init-scratch")
(load "init-dired")
(load "init-general")
(load "init-skk")
(load "init-yasnippet")
(load "init-company")
(load "init-helm")
(load "init-coding")
(load "init-flycheck")
(load "init-vc")
(load "init-cpp")
(load "init-ruby")
(load "init-perl")
(load "init-javascript")
(load "init-web")
(load "init-terraform")
(load "init-docker")
(load "init-org")
(load "init-lisp")
