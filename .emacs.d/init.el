(eval-and-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (require 'use-package))
(require 'bind-key)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(eval-and-compile
  (when (and (daemonp) (getenv "TMUX"))
    (shell-command "tmux display -p '#I' > ~/.emacs.d/emacs-server-window")
    (add-hook 'kill-emacs-hook
              (lambda ()
                (delete-file "~/.emacs.d/emacs-server-window")))))

(use-package emacs
  :bind (("RET" . newline-and-indent)
         ("C-M-r" . isearch-backward)
         ("C-M-s" . isearch-forward)
         ("C-c ]" . align-regexp)
         ("C-c h" . help-for-help)
         ("C-h" . delete-backward-char)
         ("C-r" . isearch-backward-regexp)
         ("C-s" . isearch-forward-regexp)
         ("C-x C-b" . ibuffer)
         ("M-/" . hippie-expand)
         ("M-n" . scroll-up)
         ("M-p" . scroll-down))

  :init
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (blink-cursor-mode 0)
  (setq ring-bell-function 'ignore
        initial-scratch-message nil)

  (setq custom-file (expand-file-name "custom.el" temporary-file-directory))

  (setq scroll-conservatively 35
        scroll-margin 0
        scroll-step 1)

  (setq-default tab-width 4
                indent-tabs-mode nil)

  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  :custom-face
  (default ((t (:foreground "#e4e4e4" :background "#080808"))))
  (highlight ((t (:foreground "#080808" :background "#00cd00"))))
  (region ((t (:foreground "#e4e4e4" :background "#383838"))))
  (minibuffer-prompt ((t (:foreground "#cdcd00"))))
  (mode-line ((t (:foreground "#c6c6c6" :background "#444444"))))
  (mode-line-inactive ((t (:foreground "#6c6c6c" :background "#292929"))))
  (mode-line-buffer-id ((t (:foreground "#ff8700" :weight normal))))
  (header-line ((t (:inherit mode-line :weight bold :slant italic :underline t))))
  (font-lock-comment-face ((t (:foreground "#858585" :slant italic))))
  (font-lock-string-face ((t (:foreground "#afd787"))))
  (font-lock-keyword-face ((t (:foreground "#ffaf00"))))
  (font-lock-function-name-face ((t (:foreground "#afafaf"))))
  (font-lock-variable-name-face ((t (:foreground "#87afd7"))))
  (font-lock-constant-face ((t (:foreground "#d75f5f"))))
  (font-lock-type-face ((t (:foreground "#af87ff"))))
  (font-lock-warning-face ((t (:foreground "#af0000"))))
  (font-lock-builtin-face ((t (:foreground "#d787d7")))))

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

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package rainbow-mode
  :ensure t
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

(use-package unkillable-scratch
  :ensure t
  :config
  (unkillable-scratch t))

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

(use-package dired
  :bind (:map dired-mode-map
              ("e" . wdired-change-to-wdired-mode)
              ("RET" . dired-find-alternate-file)
              ("M-o" . my:dired-open-file-by-open))
  :init
  ;; use GNU ls installed by homebrew to use its own options, not have BSD ls.
  (setq insert-directory-program "gls"
        dired-listing-switches "-AlhXF --color=auto --group-directories-first")
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :config
  ;; don't create new buffer at moving direcotry
  (put 'dired-find-alternate-file 'disabled nil)

  (defun my:dired-open-file-by-open ()
    "Open file by `open` command in dired mode."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (shell-command (concat "open " (shell-quote-argument file))))))

(use-package dired-x
  :after dired)

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-ignore-buffers-re "*[^*]+*"))

(use-package ffap
  :config
  (ffap-bindings))

(use-package minibuffer
  :custom
  (read-file-name-completion-ignore-case t))

(use-package newcomment
  :custom
  (comment-style 'extra-line))

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

(use-package autorevert
  :custom
  (auto-revert-check-vc-info t)
  :config
  (global-auto-revert-mode t))

;; cycle buffer
(defun my:switch-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") 'my:switch-last-buffer)

(use-package files
  :custom
  (backup-by-copying t)
  (version-control t)
  (kept-new-versions 5)
  (kept-old-versions 1)
  (delete-old-versions t)
  :config
  (let ((my-backup-dir (expand-file-name (format "emacs%d/backup" (user-uid)) temporary-file-directory)))
    (setq backup-directory-alist `((".*" . ,my-backup-dir)))
    (setq auto-save-file-name-transforms `((".*" ,my-backup-dir t)))
    (setq auto-save-list-file-prefix my-backup-dir)))

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
  :ensure t
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
  :ensure t
  :bind (("C-]" . er/expand-region)
         ("M-]" . er/contract-region)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-M-c" . mc/edit-lines)
         ("C-M-n" . mc/mark-next-like-this)
         ("C-M-p" . mc/mark-previous-like-this)
         ("C-M-a" . mc/mark-all-like-this)))

(use-package flyspell
  :ensure t
  :hook (prog-mode . flyspell-prog-mode)
  :custom
  (spell-extra-args ("--ignore-case"
                     "--sug-mode=ultra"
                     "--lang=en_US"
                     ;; work for camel case
                     "--run-together"
                     "--run-together-min=2"
                     "--run-together-limit=16")))

(use-package wgrep
  :ensure t)

(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs `(,(expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-global-mode 1))

(use-package company
  :ensure t
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
  (company-backends '(company-capf company-semantic company-dabbrev-code company-dabbrev company-keywords company-yasnippet))
  :custom-face
  (company-tooltip ((t (:foreground "#080808" :background "#b8b8b8"))))
  (company-tooltip-common ((t (:inherit 'company-tooltip :underline t))))
  (company-tooltip-selection ((t (:foreground "#e4e4e4" :background "#5f87af"))))
  (company-tooltip-common-selection ((t (:inherit 'company-tooltip-selection :underline t))))
  (company-preview-common ((t (:inherit 'company-tooltip-common))))
  (company-scrollbar-fg ((t (:background "#ff8700"))))
  (company-scrollbar-bg ((t (:background "#666666")))))

(use-package company-statistics
  :ensure t
  :config
  (company-statistics-mode))

(use-package projectile
  :ensure t
  :bind (("C-x f" . projectile-find-file-dwim)
         ("C-x p" . projectile-switch-project))
  :config
  (projectile-mode t))

(use-package vertico
  :ensure t
  :custom-face
  (vertico-current ((t (:background "#cb0000" :weight normal))))
  (vertico-group-title ((t (:background "#292929" :foreground "#a3a3a3" :slant italic))))
  (vertico-group-separator ((t (:inherit 'vertico-group-title))))
  :custom
  (vertico-count 15)
  :config
  (vertico-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package consult
  :ensure t
  :pin melpa-stable
  :bind (("C-x b" . consult-buffer)
         ("C-c f" . consult-find)
         ("C-c g" . consult-git-grep)
         ("C-c s" . consult-line)
         ("C-c j" . consult-imenu))
  :custom
  (consult-find-command "fd --color=never --hidden --full-path ARG OPTS")
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package helm
  :ensure t
  :bind (("C-c y" . helm-show-kill-ring))
  :custom-face
  (helm-header           ((t (:inherit 'header-line :inverse-video t))))
  (helm-source-header    ((t (:background "#292929" :foreground "#a3a3a3" :slant italic))))
  (helm-candidate-number ((t (:foreground "#5fafff" :background "#444444"))))
  (helm-selection        ((t (:background "#cb0000" :weight normal))))
  (helm-match            ((t (:foreground "#a2cd5a")))))

(use-package orderless
  :ensure t
  :pin melpa-stable
  :custom
  (completion-styles '(orderless)))

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
  :ensure t
  :bind (("C-c ." . dash-at-point)
         ("C-c C-." . dash-at-point-with-docset)))

(use-package sql
  :config
  (sql-highlight-mysql-keywords))

(use-package markdown-mode
  :ensure t
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
  :ensure t
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-default-project nil)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package jsonnet-mode)

(use-package flycheck
  :ensure t
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :custom-face
  (flycheck-warning ((t (:underline t :weight normal :slant italic))))
  (flycheck-error   ((t (:underline t :weight bold :slant italic))))
  :config
  (global-flycheck-mode t))

(use-package flycheck-popup-tip
  :ensure t
  :config
  (flycheck-popup-tip-mode)
  :custom-face
  (popup-tip-face ((t (:foreground "#5fafd7" :background "#303030")))))

(require 'vc-git)
(require 'vc-dir)
(require 'vc-annotate)

(setq vc-follow-symlinks t)
(setq vc-make-backup-files t)

;; make compact vc-annotate display
(defadvice vc-git-annotate-command (around vc-git-annotate-command activate)
  "Suppress relative path of file from git blame output."
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=short" rev "--" name)))

;; open Pull Reuqest URL on this line from vc-annotate enter P as same as tig
(defun my:open-pr-at-line ()
  "Open Pull Request URL at the line from git blame output."
  (interactive)
  (let* ((rev-at-line (vc-annotate-extract-revision-at-line))
         (rev (car rev-at-line)))
    (shell-command (concat "git hub open " rev))))
(define-key vc-annotate-mode-map (kbd "8") 'my:open-pr-at-line)

;; open current file by tig with blame mode
(defun my:tig-current-file ()
  (interactive)
  (shell-command
   (format "tmux new-window 'cd %s && tig blame +%s %s'"
           (file-name-directory buffer-file-name)
           (line-number-at-pos)
           (file-name-nondirectory buffer-file-name))))
(define-key vc-prefix-map [(t)] 'my:tig-current-file)

;; https://snarfed.org/emacs-vc-git-tweaks
;;
;; In vc-git and vc-dir for git buffers, make (C-x v) a run git add, u run git
;; reset, and r run git reset and checkout from head.
(defun my:vc-git-command (verb fn)
  (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset nil t)))
         (backend (car fileset-arg))
         (files (nth 1 fileset-arg)))
    (if (eq backend 'Git)
        (progn (funcall fn files)
               (message (concat verb " " (number-to-string (length files))
                                " file(s).")))
      (message "Not in a vc git buffer."))))

(defun my:vc-git-add (&optional revision vc-fileset comment)
  (interactive "P")
  (my:vc-git-command "Staged" 'vc-git-register))
(define-key vc-prefix-map [(a)] 'my:vc-git-add)
(define-key vc-dir-mode-map [(a)] 'my:vc-git-add)

(defun my:vc-git-reset (&optional revision vc-fileset comment)
  (interactive "P")
  (my:vc-git-command "Unstaged"
                     (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))
(define-key vc-prefix-map [(u)] 'my:vc-git-reset)
(define-key vc-dir-mode-map [(u)] 'my:vc-git-reset)
;; Remap vc-revert to `r` from `u`
(define-key vc-prefix-map [(r)] 'vc-revert)
(define-key vc-dir-mode-map [(r)] 'vc-revert)

;; hide up to date files after refreshing in vc-dir
(define-key vc-dir-mode-map [(g)]
  (lambda () (interactive) (vc-dir-refresh) (vc-dir-hide-up-to-date)))

;; enable aut-fill-mode at git commit editting for 50/72 rules
(add-hook 'vc-git-log-edit-mode-hook (lambda ()
                                       (setq fill-column 72)
                                       (turn-on-auto-fill)))

(use-package git-commit
  :ensure t
  :config
  (global-git-commit-mode)
  :custom
  ;; 50/72 rules
  (git-commit-summary-max-length 50)
  (git-commit-fill-column 72))

(use-package browse-at-remote
  :ensure t
  :config
  (define-key vc-prefix-map [(w)] 'browse-at-remote))

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-c v" . magit-file-dispatch)))

(use-package lsp-mode
  :ensure t
  :commands
  (lsp lsp-deferred)
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  :config
  ;; Use terraform-ls instead of terraform-lsp to be stable
  ;; https://github.com/hashicorp/terraform-ls/blob/master/docs/USAGE.md#emacs
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
                    :major-modes '(terraform-mode)
                    :server-id 'terraform-ls)))

;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun my:lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :bind (:map go-mode-map
         ;; map `C-=` to default ascii codes `^[[61;5u` by iTerm's key map
         ("C-=" . my:insert-short-var-declaration-op))
  :hook
  (go-mode . lsp-deferred)
  (go-mode . my:lsp-go-install-save-hooks)
  :config
  (defun my:insert-short-var-declaration-op ()
    "Insert `:=` at the point"
    (interactive)
    (unless (string= (string (char-before)) " ")
      (insert " "))
    (insert ":=")
    (unless (string= (string (char-after)) " ")
      (insert " "))))

(use-package ruby-mode
  :interpreter "ruby"
  :custom
  (ruby-insert-encoding-magic-comment nil))

(use-package ruby-end
  :ensure t
  :hook
  (ruby-mode . ruby-end-mode))

(use-package rspec-mode
  :ensure t
  :custom
  (rspec-use-relative-path t)
  :config
  (rspec-install-snippets))

(use-package inf-ruby
  :ensure t
  :hook
  ((ruby-mode . inf-ruby-minor-mode)
   (ruby-mode . inf-ruby-switch-setup)))

(use-package rbenv
  :ensure t
  :config
  (global-rbenv-mode))

(use-package projectile-rails
  :ensure t
  :bind-keymap
  ("C-c r" . projectile-rails-command-map)
  :config
  (projectile-rails-global-mode))

(use-package haml-mode)

(use-package cperl-mode
  :mode ("\\.t\\'" "\\.psgi\\'" "cpanfile")
  :interpreter "perl"
  :bind (:map cperl-mode-map
         ("M-?" . cperl-perldoc-at-point)
         ("C-c ." . cperl-perldoc)
         ("C-c t" . my:perltidy-region)
         ("C-c T" . my:perltidy-buffer))
  :custom
  (cperl-close-paren-offset -4)
  (cperl-continued-statement-offset 4)
  (cperl-indent-level 4)
  (cperl-label-offset -4)
  (cperl-indent-parens-as-block t)
  (cperl-tab-always-indent t)
  (cperl-auto-newline nil)
  (cperl-electric-linefeed nil)
  (cperl-autoindent-on-semi t)
  (cperl-highlight-variables-indiscriminately t)
  (cperl-font-lock t)
  :custom-face
  (cperl-array-face ((t (:inherit font-lock-variable-name-face))))
  (cperl-hash-face ((t (:inherit font-lock-variable-name-face))))
  (cperl-nonoverridable-face ((t (:foreground "#d7d700"))))
  :config
  (defalias 'perl-mode 'cperl-mode)

  (font-lock-add-keywords 'cperl-mode '(("state" . font-lock-keyword-face)))

  (defun my:perltidy-region (beg end)
    (interactive "r")
    (shell-command-on-region beg end "perltidy -q" nil t))

  (defun my:perltidy-buffer (buffer)
    "Run the perltidy formatter on the buffer."
    (interactive (list (current-buffer)))
    (with-current-buffer buffer
      (my:perltidy-region (point-min) (point-max))))

  ;; ffap with perldoc
  (defun my:ffap-cperl-mode (file)
    (let ((real-file (shell-command-to-string (concat "perldoc -lm " file))))
      (unless (string-match "No module found for " real-file)
        (substring real-file (string-match "/" real-file) -1))))
  (add-to-list 'ffap-alist '(cperl-mode . my:ffap-cperl-mode)))

(use-package plenv)

;; flycheck with Project::Libs
(flycheck-define-checker perl-project-libs
  "A perl syntax checker with Project::Libs."
  :command ("plenv"
            "exec"
            "perl"
            "-MProject::Libs lib_dirs => [qw(local/lib/perl5)]"
            "-wc"
            source-inplace)
  :error-patterns ((error line-start
                          (minimal-match (message)) " at " (file-name) " line " line
                          (or "." (and ", " (zero-or-more not-newline)))
                          line-end))
  :modes (cperl-mode))

(add-hook 'cperl-mode-hook
          (lambda ()
            (flycheck-mode t)
            (setq flycheck-checker 'perl-project-libs)))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'"  . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter "node"
  :custom
  (js2-basic-offset 2))

(use-package json-mode
  :ensure t)

(use-package typescript-mode
  :ensure t
  :custom
  (typescript-indent-level 2))

(use-package coffee-mode
  :mode "\\.coffee\\'"
  :custom
  (coffee-tab-width 2)
  (coffee-indent-like-python-mode t))

(use-package prettier-js
  :hook
  (js2-mode . prettier-js))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.tmpl\\'" "\\.tt\\'" "\\.tx\\'")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-style-padding 2)
  (web-mode-script-padding 2)
  (web-mode-engines-alist '(("template-toolkit" . "\\.tt\\'")
                            ("template-toolkit" . "\\.tx\\'"))))

(use-package css-mode
  :custom
  (css-indent-offset 2))

(use-package scss-mode
  :ensure t
  :mode ("\\.css\\'" "\\.scss\\'")
  :custom
  (scss-compile-at-save nil))

(defun my:escape-html-region (start end)
  "Escape '&<>' characters in the region using '&amp;', '&lt;', and '&gt;'."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (search-forward "&" nil t)
        (replace-match "&amp;" nil t))
      (goto-char start)
      (while (search-forward "<" nil t)
        (replace-match "&lt;" nil t))
      (goto-char start)
      (while (search-forward ">" nil t)
        (replace-match "&gt;" nil t)))))

(use-package terraform-mode
  :ensure t
  :hook
  ((terraform-mode . terraform-format-on-save-mode)
   (terraform-mode . lsp-deferred)))

(use-package yaml-mode
  :ensure t
  :bind (:map yaml-mode-map
         ("C-m" . newline-and-indent)))

(use-package docker-compose-mode)

(use-package dockerfile-mode
  :ensure t)

(use-package org
  :bind (("C-c c" . org-capture))
  :custom
  (org-directory "~/Dropbox/org/")
  (org-default-notes-file (concat org-directory "notes.org"))
  (org-startup-truncated nil)
  (org-startup-folded nil)
  (org-return-follows-link t)
  (org-src-fontify-natively t) ;; hilight lines in code block
  (org-log-done 'time) ;; insert `CLOSED [timestamp]` after the headline
  :init
  ;; blog template
  (defun my:blog-subtree-post-capture-template ()
    "Returns `org-capture' template for new blog post."
    (let ((section (format-time-string "posts/%Y/%m/" (org-current-time)))
          (date (format-time-string "%Y-%m-%d" (org-current-time))))
      (mapconcat 'identity
                 `("** TODO %?"
                   "  :PROPERTIES:"
                   "  :EXPORT_FILE_NAME: "
                   ,(concat "  :EXPORT_DATE: " date)
                   ,(concat "  :EXPORT_HUGO_SECTION: " section)
                   "  :END:"
                   "\n")
                 "\n")))
  ;; org-capture
  (setq org-capture-templates
        '(("n" "Notes" entry (file+datetree (lambda () (concat org-directory "notes.org")))
           "* [%<%H:%M>] %?\n")
          ("b" "Blog Posts" entry (file+olp "~/src/ziguzagu.org/blog.org" "Blog Posts")
           (function my:blog-subtree-post-capture-template)
           :prepend t :empty-lines 1)))
  :config
  (require 'org-tempo))

(use-package ox-hugo
  :after ox)

(use-package slime)

(use-package slime-company)

(eval-and-compile
  (let ((host-local-config (expand-file-name "init-local.el" user-emacs-directory)))
    (when (file-exists-p host-local-config)
      (load host-local-config))))
