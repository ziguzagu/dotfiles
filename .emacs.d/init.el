(setq debug-on-error t)

(setq inhibit-startup-message t)

(setq custom-file (expand-file-name "custom.el" temporary-file-directory))

(eval-and-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (setq package-archive-priorities '(("melpa" . 10)
                                     ("melpa-stable" . 5)
                                     ("gnu" . 1)))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))
(require 'bind-key)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(add-to-list 'load-path "~/.emacs.d/init")
(load "init-server")
(load "init-appearance")
(load "init-modeline")
(load "init-scratch")
(load "init-dired")
(load "init-general")
(load "init-yasnippet")
(load "init-company")
(load "init-helm")
(load "init-coding")
(load "init-flycheck")
(load "init-vc")
(load "init-lsp")
(load "init-go")
(load "init-ruby")
(load "init-perl")
(load "init-javascript")
(load "init-web")
(load "init-terraform")
(load "init-docker")
(load "init-org")
(load "init-lisp")
