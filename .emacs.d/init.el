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

(eval-and-compile
  (when (and (daemonp) (getenv "TMUX"))
    (shell-command "tmux display -p '#I' > ~/.emacs.d/emacs-server-window")
    (add-hook 'kill-emacs-hook
              (lambda ()
                (delete-file "~/.emacs.d/emacs-server-window")))))

(eval-and-compile
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (blink-cursor-mode 0)
  (setq ring-bell-function 'ignore)

  (set-face-attribute 'default nil
                      :foreground "#e4e4e4"
                      :background "#080808")
  (set-face-attribute 'highlight nil
                      :foreground "#080808"
                      :background "#00cd00")
  (set-face-attribute 'region nil
                      :foreground "#e4e4e4"
                      :background "#383838")
  (set-face-attribute 'minibuffer-prompt nil
                      :foreground "#cdcd00")
  (set-face-attribute 'mode-line nil
                      :foreground "#c6c6c6"
                      :background "#444444")
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#6c6c6c"
                      :background "#292929")
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground "#ff8700"
                      :weight 'normal)
  (set-face-attribute 'header-line nil
                      :inherit 'mode-line
                      :weight 'bold
                      :slant 'italic
                      :underline t)

  (set-face-attribute 'font-lock-comment-face nil
                      :foreground "#858585"
                      :slant 'italic)
  (set-face-attribute 'font-lock-string-face nil
                      :foreground "#afd787")
  (set-face-attribute 'font-lock-keyword-face nil
                      :foreground "#ffaf00")
  (set-face-attribute 'font-lock-function-name-face nil
                      :foreground "#afafaf")
  (set-face-attribute 'font-lock-variable-name-face nil
                      :foreground "#87afd7")
  (set-face-attribute 'font-lock-constant-face nil
                      :foreground "#d75f5f")
  (set-face-attribute 'font-lock-type-face nil
                      :foreground "#af87ff")
  (set-face-attribute 'font-lock-warning-face nil
                      :foreground "#af0000")
  (set-face-attribute 'font-lock-builtin-face nil
                      :foreground "#d787d7"))

(use-package whitespace
  :custom
  (show-trailing-whitespace t)
  (whitespace-style '(face tabs tab-mark trailing))
  (whitespace-global-modes '(not go-mode))
  :config
  (global-whitespace-mode t)
  :custom-face
  (trailing-whitespace ((t (:foreground "#e5e5e5" :background "#525252"))))
  (whitespace-trailing ((t (:inherit trailing-whitespace))))
  (whitespace-tab ((t (:foreground "#666666" :background nil)))))

(use-package rainbow-mode
  :hook (emacs-lisp-mode . rainbow-mode))

(add-to-list 'load-path "~/.emacs.d/init")
(load "init-modeline")
(load "init-scratch")
(load "init-dired")
(load "init-general")
(load "init-yasnippet")
(load "init-company")
(load "init-completion")
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

(eval-and-compile
  (let ((host-local-config "~/.emacs.d/init-local.el"))
    (when (file-exists-p host-local-config)
      (load host-local-config))))
