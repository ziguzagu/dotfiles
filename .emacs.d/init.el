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

(eval-and-compile
  (make-face 'mode-line-vc-mode)
  (make-face 'fc-info-face)
  (make-face 'fc-warning-face)
  (make-face 'fc-error-face)
  (set-face-attribute 'mode-line-vc-mode nil
                      :foreground "#5fafff"
                      :weight 'normal)
  (set-face-attribute 'fc-info-face nil
                      :foreground "#83a598"
                      :weight 'normal)
  (set-face-attribute 'fc-warning-face nil
                      :inherit 'fc-info-face
                      :foreground "#fabd2f")
  (set-face-attribute 'fc-error-face nil
                      :inherit 'fc-info-face
                      :foreground "#fb4933")

  ;; get rid of leading ' git:' from vc-mode
  (defun my:vc-branch ()
    (let ((backend (vc-backend buffer-file-name)))
      (substring vc-mode 5)))

  ;; customize flycheck modeline display
  (defun my:mode-line-checker-text (state)
    (let* ((counts (flycheck-count-errors flycheck-current-errors))
           (errorp (flycheck-has-current-errors-p state))
           (err (or (cdr (assq state counts)) "?"))
           (running (eq 'running flycheck-last-status-change)))
      (if (or errorp running) (format "•%s" err))))

  (defun my:mode-line-chcker ()
    (when (and (bound-and-true-p flycheck-mode)
               (or flycheck-current-errors
                   (eq 'running flycheck-last-status-change)))
      (cl-loop for state in '(error warning info)
               as ret = (my:mode-line-checker-text state)
               when ret
               concat (propertize
                       ret
                       'face (intern (format "fc-%S-face" state))))))

  (setq-default mode-line-format
                (list " "
                      'mode-line-mule-info
                      'mode-line-modified
                      "  "
                      'mode-line-buffer-identification
                      '(:eval (when (fboundp 'projectile-project-name)
                                (format " [%s]" (projectile-project-name))))
                      '(vc-mode
                        ((:propertize "  " face mode-line-vc-mode)
                         (:propertize (:eval (my:vc-branch)) face mode-line-vc-mode)))
                      "  "
                      'mode-name
                      "  "
                      '(:eval (my:mode-line-chcker))
                      "  %c:%l(%p)")))

(setq initial-scratch-message nil)

(use-package unkillable-scratch
  :config (unkillable-scratch t))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

(defun my:dired-open-file-by-open ()
  "Open file by `open` command in dired mode."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (shell-command (concat "open " (shell-quote-argument file)))))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("e" . wdired-change-to-wdired-mode)
              ("RET" . dired-find-alternate-file)
              ("M-o" . my:dired-open-file-by-open))
  :init
  ;; use GNU ls installed by homebrew to use its own options, not have BSD ls.
  (setq insert-directory-program "gls"
        dired-listing-switches "-AlhXF --color=auto --group-directories-first")
  ;; recursive copy/delete
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)
  ;; don't create new buffer at moving direcotry
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  ;; use dired-jump
  (use-package dired-x
    :ensure nil
    :bind ("C-x C-d" . dired-jump)))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;; using ffap
(ffap-bindings)
(setq read-file-name-completion-ignore-case t)

(add-to-list 'load-path "~/.emacs.d/init")
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
