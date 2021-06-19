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

;; scroll by line
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; scroll up and down
(global-set-key (kbd "M-p") 'scroll-down)
(global-set-key (kbd "M-n") 'scroll-up)

;; enable upcase/downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; key bind
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c h") 'help-for-help)
(global-set-key (kbd "C-h")  'delete-backward-char)
(global-set-key (kbd "C-c ]") 'align-regexp)
;; C-h as delete in mini buffer
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

(defun my:delete-word-at-point ()
  "Delete the word at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (kill-region (car bounds) (cdr bounds)))))
(global-set-key (kbd "M-d") 'my:delete-word-at-point)

(use-package recentf
  :custom
  (recentf-max-saved-items 5000)
  :config
  (recentf-mode 1))

;; reload a file when it was changed by another process (include vc)
(global-auto-revert-mode t)
(setq auto-revert-check-vc-info t)

;; cycle buffer
(defun my:switch-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") 'my:switch-last-buffer)

;; backup
(defconst my-backup-dir (expand-file-name (format "emacs%d/backup" (user-uid)) temporary-file-directory))
(setq backup-directory-alist `((".*" . ,my-backup-dir))
      auto-save-file-name-transforms `((".*" ,my-backup-dir t))
      auto-save-list-file-prefix my-backup-dir)
(setq backup-by-copying t
      version-control t
      kept-new-versions 5
      kept-old-versions 1
      delete-old-versions t)

;; rotate window divide vertical / horizontal
(defun my:window-toggle-split ()
  "toggle splitted windows vertical and horizontal"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "no splitted windows"))
  (let (before-height (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally))
    (switch-to-buffer-other-window other-buf)
    (other-window -1)))
(global-set-key (kbd "C-x 9") 'my:window-toggle-split)

;; split window or move other window by one keybind
(defun my:other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-z") 'my:other-window-or-split)

(use-package popwin
  :custom
  (popwin:popup-window-position 'bottom)
  (popwin:popup-window-height 20)
  :config
  (popwin-mode 1))

(eval-and-compile
  (when (eq system-type 'darwin)
    (defun my:copy-from-osx ()
      "Get clipboard contents."
      (shell-command-to-string "pbpaste"))

    (defun my:paste-to-osx (text &optional push)
      "Paste yanked contents to clipboard."
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (setq interprogram-cut-function 'my:paste-to-osx)
    (setq interprogram-paste-function 'my:copy-from-osx)))

(use-package expand-region
  :bind (("C-]" . er/expand-region)
         ("M-]" . er/contract-region)))

(use-package multiple-cursors
  :bind (("C-M-c" . mc/edit-lines)
         ("C-M-n" . mc/mark-next-like-this)
         ("C-M-p" . mc/mark-previous-like-this)
         ("C-M-a" . mc/mark-all-like-this)))

(use-package flyspell
  :init
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--ignore-case"
                            "--sug-mode=ultra"
                            "--lang=en_US"
                            ;; work for camel case
                            "--run-together"
                            "--run-together-min=2"
                            "--run-together-limit=16"))
  :hook (prog-mode . flyspell-prog-mode))

(use-package wgrep)

(use-package yasnippet
  :bind (("C-c i" . yas-expand))
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package company
  :bind (("C-o" . company-dabbrev)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-filter-candidates)
         ("TAB" . company-complete-selection)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (global-company-mode t)
  :custom
  (company-idle-delay 0)
  (company-auto-expand t)
  (company-minimum-prefix-length 3)
  (company-dabbrev-minimum-length 3)
  ;; go to top at the next of last candidates
  (company-selection-wrap-around t)
  (company-backends '(company-capf company-semantic company-dabbrev-code company-dabbrev company-keywords))
  :custom-face
  (company-tooltip ((t (:foreground "#080808" :background "#b8b8b8"))))
  (company-tooltip-common ((t (:inherit 'company-tooltip :underline t))))
  (company-tooltip-selection ((t (:foreground "#e4e4e4" :background "#5f87af"))))
  (company-tooltip-common-selection ((t (:inherit 'company-tooltip-selection :underline t))))
  (company-preview-common ((t (:inherit 'company-tooltip-common))))
  (company-scrollbar-fg ((t (:background "#ff8700"))))
  (company-scrollbar-bg ((t (:background "#666666")))))

(use-package company-statistics
  :config
  (company-statistics-mode))

(use-package vertico
  :pin melpa-stable
  :config
  (vertico-mode))

(use-package consult
  :pin melpa-stable
  :bind (("C-c s" . consult-imenu)))

(use-package orderless
  :pin melpa-stable
  :custom
  (completion-styles '(orderless)))

(eval-and-compile
  (setq-default tab-width 4
                indent-tabs-mode nil)

  (setq comment-style 'extra-line))

(use-package diff-mode
  :custom-face
  (diff-header         ((t (:foreground "#a8a8a8" :background "#303030" :slant italic))))
  (diff-file-header    ((t (:inherit diff-header))))
  (diff-hunk-header    ((t (:inherit diff-header :background "#080808"))))
  (diff-index          ((t (:inherit diff-hunk-header :foreground "#5fafd7"))))
  (diff-function       ((t (:inherit diff-hunk-header :foreground "#af87d7"))))
  (diff-context        ((t (:inherit default))))
  (diff-added          ((t (:inherit default :foreground "#87af5f"))))
  (diff-removed        ((t (:inherit default :foreground "#d75f5f"))))
  (diff-refine-added   ((t (:inherit diff-added :background "#005f00"))))
  (diff-refine-removed ((t (:inherit diff-removed :background "#5f0000")))))

(use-package dash-at-point
  :bind (("C-c ." . dash-at-point)
         ("C-c C-." . dash-at-point-with-docset)))

(use-package sql
  :config
  (sql-highlight-mysql-keywords))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :custom-face
  (markdown-header-delimiter-face ((t (:foreground "#ffa500"))))
  (markdown-header-rule-face      ((t (:foreground "#ffa500"))))
  (markdown-header-face           ((t (:foreground "#ffa500"))))
  (markdown-inline-code-face      ((t (:foreground "#87d75f"))))
  (markdown-pre-face              ((t (:foreground "#87d75f"))))
  (markdown-language-keyword-face ((t (:foreground "#858585"))))
  (markdown-list-face             ((t (:foreground "#af87ff" :weight bold))))
  (markdown-link-face             ((t (:foreground "#5fafff")))))

(use-package sh-script
  :custom
  (sh-shell-file "/bin/zsh"))

(use-package dumb-jump
  :bind (("M-." . dumb-jump-go)
         ("M-," . dumb-jump-back))
  :init
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-selector 'helm)
  (setq dumb-jump-default-project nil))

(use-package jsonnet-mode
  :defer t)

(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode t)
  :custom-face
  (flycheck-warning ((t (:underline t :weight normal :slant italic))))
  (flycheck-error   ((t (:underline t :weight bold :slant italic)))))

(use-package flycheck-popup-tip
  :config
  (flycheck-popup-tip-mode)
  :custom-face
  (popup-tip-face ((t (:foreground "#5fafd7" :background "#303030")))))

(add-to-list 'load-path "~/.emacs.d/init")
;(load "init-helm")
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
