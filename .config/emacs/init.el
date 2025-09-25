(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (require 'use-package))

;; prevent to pop up *Warnings* and *Compile-Log* buffers because it is not useful for me
(add-to-list 'display-buffer-alist
  '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
     (display-buffer-no-window)
     (allow-no-window . t)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(eval-and-compile
  (when (and (daemonp) (getenv "TMUX"))
    (shell-command (concat "tmux display -p '#I' > " (expand-file-name "emacs-server-window" temporary-file-directory)))
    (add-hook 'kill-emacs-hook
      (lambda ()
        (delete-file (expand-file-name "emacs-server-window" temporary-file-directory))))))

(use-package emacs
  :ensure nil
  :bind (("RET" . newline-and-indent)
          ("C-c h" . help-for-help)
          ("C-g" . my:keyboard-quit-dwim)
          ("C-h" . delete-backward-char)
          ("C-r" . isearch-backward-regexp)
          ("C-s" . isearch-forward-regexp)
          ("C-x C-b" . ibuffer)
          ("M-/" . hippie-expand)
          ("M-d" . my:delete-word-at-point)
          ("M-n" . scroll-up)
          ("M-p" . scroll-down))
  :custom
  (scroll-conservatively 35)
  (scroll-margin 0)
  (scroll-step 1)
  (tab-always-indent 'complete)
  (ns-command-modifier 'meta)  ;; make Command ⌘ to Meta
  (ns-option-modifier 'super)  ;; make Option ⌥ to Super
  :custom-face
  (default ((t (:family "Source Han Code JP" :height 120 :weight regular))))
  (fixed-pitch ((t (:inherit default))))
  :init
  (setq custom-file (expand-file-name "custom.el" temporary-file-directory))
  :config
  (menu-bar-mode -1)

  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)

  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (set-fontset-font t nil "Source Han Code JP")

  (defun my:delete-word-at-point ()
    "Delete the word at point."
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when bounds
        (kill-region (car bounds) (cdr bounds)))))

  ;; make C-g a bit more helpful:
  ;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
  (defun my:keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'."
    (interactive)
    (cond
      ((region-active-p)
        (keyboard-quit))
      ((derived-mode-p 'completion-list-mode)
        (delete-completion-window))
      ((> (minibuffer-depth) 0)
        (abort-recursive-edit))
      (t
        (keyboard-quit))))

  (when (eq system-type 'darwin)
    (defun my:copy-from-macos ()
      "Get clipboard contents."
      (let ((pbpaste (purecopy "pbpaste"))
             (tramp-mode nil)
             (default-directory "~"))
        (shell-command-to-string "pbpaste")))
    (setq interprogram-cut-function 'my:paste-to-macos)

    (defun my:paste-to-macos (text &optional push)
      "Paste yanked contents to clipboard."
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))
    (setq interprogram-paste-function 'my:copy-from-macos)))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package nerd-icons
  :ensure t
  :config
  (when (and (eq system-type 'darwin) (display-graphic-p))
    (let ((font-file (expand-file-name "~/Library/Fonts/NFM.ttf")))
      (unless (file-exists-p font-file)
        (nerd-icons-install-fonts t)))))

(use-package nerd-icons-completion
  :ensure t
  :hook
  (after-init . nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-env-version nil))

(use-package treesit
  :ensure nil
  :when (treesit-available-p)
  :init
  (setq treesit-language-source-alist
    '((css "https://github.com/tree-sitter/tree-sitter-css")
       (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
       (json "https://github.com/tree-sitter/tree-sitter-json")
       (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
       (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
       (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
       (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; install grammars
  (dolist (lang '(css javascript json ruby tsx typescript yaml))
    (unless (treesit-language-available-p lang)
      (ignore-errors
        (treesit-install-language-grammar lang)))))

(use-package whitespace
  :ensure nil
  :custom
  (whitespace-style '(face tabs tab-mark trailing))
  (whitespace-global-modes '(not dired-mode go-mode eat-mode vterm-mode))
  :config
  (global-whitespace-mode t))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode 1))

(use-package rainbow-mode
  :ensure t
  :hook
  (emacs-lisp-mode . rainbow-mode))

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "rsync")
  ;; Improve tramp performance:
  ;; * https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  ;; * https://www.gnu.org/software/tramp/tramp-emacs.html
  (tramp-verbose 1)
  (tramp-completion-reread-directory-timeout nil)
  (tramp-chunksize 500))

(use-package unkillable-scratch
  :ensure t
  :config
  (unkillable-scratch t))

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

(use-package replace
  :ensure nil
  :bind (:prefix-map my:replace-command-map
          :prefix-docstring "My replace command map"
          :prefix "C-c q"
          ("q" . query-replace)
          ("r" . query-replace-regexp)
          ("s" . replace-string)))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
          ("e" . wdired-change-to-wdired-mode)
          ("RET" . dired-find-alternate-file)
          ("M-o" . my:dired-open-file-by-open))
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . hl-line-mode)
  :custom
  (dired-listing-switches "-AlhXF --color=auto --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (wdired-allow-to-change-permissions t)
  :config
  ;; don't create new buffer at moving directory
  (put 'dired-find-alternate-file 'disabled nil)

  (defun my:dired-open-file-by-open ()
    "Open file by `open` command in dired mode."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (shell-command (concat "open " (shell-quote-argument file))))))

(use-package dired-x
  :ensure nil)

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
          ("TAB" . dired-subtree-toggle))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-ignore-buffers-re "*[^*]+*"))

(use-package ffap
  :ensure nil
  :custom
  (ffap-machine-p-known 'reject)
  :config
  (ffap-bindings))

(use-package minibuffer
  :ensure nil
  :custom
  (read-file-name-completion-ignore-case t)
  :config
  (define-key key-translation-map (kbd "C-h") (kbd "<DEL>")))

(use-package newcomment
  :ensure nil
  :custom
  (comment-style 'extra-line))

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 5000)
  :config
  (recentf-mode 1))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-check-vc-info t)
  :config
  (global-auto-revert-mode t))

(use-package files
  :ensure nil
  :custom
  (backup-by-copying t)
  (version-control t)
  (kept-new-versions 5)
  (kept-old-versions 1)
  (delete-old-versions t)
  ;; Find .dir-locals.el in remote hosts by tramp
  (enable-remote-dir-locals t)
  ;; Improve tramp performance: https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (remote-file-name-inhibit-locks t)
  :config
  ;; macOS ls doesn't support some options like --group-directories-first, so use coreutils ls both on macOS and Linux
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls"))
  ;; I don't want to see backup files in the same directory
  (let ((my-backup-dir (expand-file-name (format "emacs%d/backup" (user-uid)) temporary-file-directory)))
    (setq backup-directory-alist `((".*" . ,my-backup-dir)))
    (setq auto-save-file-name-transforms `((".*" ,my-backup-dir t)))
    (setq auto-save-list-file-prefix my-backup-dir)))

(use-package window
  :ensure nil
  :bind (("C-c b" . my:switch-last-buffer)
          ("C-x 9" . my:rotate-windows)
          ("C-x o" . my:other-window-or-split))
  :config
  (defun my:rotate-windows ()
    "Rotate split windows vertical and horizontal."
    (interactive)
    (unless (= (count-windows 1) 2)
      (error "No split windows"))
    (let (before-height (other-buf (window-buffer (next-window))))
      (setq before-height (window-height))
      (delete-other-windows)
      (if (= (window-height) before-height)
        (split-window-vertically)
        (split-window-horizontally))
      (switch-to-buffer-other-window other-buf)
      (other-window -1)))

  (defun my:switch-last-buffer ()
    "Switch to last buffer."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

  (defun my:other-window-or-split ()
    "Move into other window or split window."
    (interactive)
    (when (one-window-p)
      (split-window-horizontally))
    (other-window 1)))

(use-package popwin
  :ensure t
  :custom
  (popwin:popup-window-position 'bottom)
  (popwin:popup-window-height 20)
  :config
  (popwin-mode 1))

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
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--ignore-case"
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

(use-package yasnippet-snippets
  :ensure t)

(use-package corfu
  :ensure t
  :bind (:map corfu-map
          ("TAB" . corfu-complete)
          ("RET" . nil))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-cycle t)
  (corfu-max-width 100)
  (corfu-scroll-margin 4)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(0.8 . 0.3))
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode))

(use-package cape
  :ensure t
  :bind ("C-o" . cape-dabbrev)
  :custom
  (cape-dabbrev-min-length 3)
  (cape-dabbrev-check-other-buffers t)
  :config
  ;; Add global completion functions
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)

  ;; Add mode-specific completions
  (add-hook 'emacs-lisp-mode-hook
    (lambda ()
      (add-hook 'completion-at-point-functions #'cape-elisp-symbol -10 t))))

(use-package projectile
  :ensure t
  :bind ("C-x f" . projectile-find-file-dwim)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 15)
  :config
  (vertico-mode))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
          ("C-c f" . consult-flymake)
          ("C-c g" . my:consult-git-grep-dwim)
          ("C-c s" . consult-line)
          ("C-c j" . consult-imenu))
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  ;; Start consult-git-grep with active region
  ;; see also: https://github.com/minad/consult/wiki#start-consult-ripgrep-search-with-active-region
  (defun my:consult-git-grep-dwim ()
    "Pass the region to consult-git-grep if available."
    (interactive)
    (if (use-region-p)
      (let ((initial (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark t)
        (consult-git-grep nil initial))
      (consult-git-grep))))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  ;; disable default completion styles to use orderless for every completions
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package browse-kill-ring
  :ensure t
  :bind (("C-c y" . browse-kill-ring)))

(use-package sql
  :ensure nil
  :config
  (sql-highlight-mysql-keywords))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode))

(use-package sh-script
  :ensure nil
  :custom
  (sh-shell-file "/bin/zsh"))

(use-package emacs-lisp
  :ensure nil
  :bind (:map emacs-lisp-mode-map
          ("C-c h ." . my:describe-symbol-at-point))
  :hook
  (emacs-lisp-mode . my:disable-flycheck-in-init)
  :init
  (defun my:describe-symbol-at-point ()
    "Describe the function or variable at point."
    (interactive)
    (let* ((sym (symbol-at-point)))
      (if (not sym)
        (message "No valid symbol at point")
        (cond
          ((fboundp sym) (describe-function sym))
          ((boundp sym) (describe-variable sym))
          (t (message "Symbol `%s' is neither a function nor a variable" sym))))))

  (defun my:disable-flycheck-in-init ()
    "Disable specific flycheck checkers in init.el."
    (when (and buffer-file-name
            (string-equal (file-truename buffer-file-name) (file-truename user-init-file)))
      (setq-local flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))))

(use-package vc
  :ensure nil
  :bind (:map vc-prefix-map
          ("t" . my:tig-current-file)
          ("a" . my:vc-git-add)
          ("u" . my:vc-git-reset)
          ("r" . vc-revert))
  :hook
  (vc-git-log-edit-mode . my:vc-git-log-edit-setup)
  :custom
  (vc-follow-symlinks t)
  (vc-make-backup-files t)
  ;; Limit vc backends to Git only to improve performance of tramp:)
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (vc-handled-backends '(Git))
  :config
  (defun my:tig-current-file ()
    "Open current file by tig with blame mode."
    (interactive)
    (shell-command
      (format "tmux new-window 'cd %s && tig blame +%s %s'"
        (file-name-directory buffer-file-name)
        (line-number-at-pos)
        (file-name-nondirectory buffer-file-name))))

  (defun my:vc-git-command (verb fn)
    "Execute git command with proper messaging."
    (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset nil t)))
            (backend (car fileset-arg))
            (files (nth 1 fileset-arg)))
      (if (eq backend 'Git)
        (progn (funcall fn files)
          (message (concat verb " " (number-to-string (length files))
                     " file(s).")))
        (message "Not in a vc git buffer."))))

  (defun my:vc-git-add (&optional revision vc-fileset comment)
    "Stage files with git add."
    (interactive "P")
    (my:vc-git-command "Staged" 'vc-git-register))

  (defun my:vc-git-reset (&optional revision vc-fileset comment)
    "Unstage files with git reset."
    (interactive "P")
    (my:vc-git-command "Unstaged"
      (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))

  (defun my:vc-git-log-edit-setup ()
    "Setup for git commit editing with 50/72 rules."
    (setq fill-column 72)
    (auto-fill-mode 1)))

(use-package vc-dir
  :ensure nil
  :bind (:map vc-dir-mode-map
          ("a" . my:vc-git-add)
          ("u" . my:vc-git-reset)
          ("g" . my:vc-dir-refresh-and-hide-up-to-date))
          ("r" . vc-revert)
  :config
  (defun my:vc-dir-refresh-and-hide-up-to-date ()
    "Refresh vc-dir and hide up-to-date files."
    (interactive)
    (vc-dir-refresh)
    (vc-dir-hide-up-to-date)))

(use-package vc-annotate
  :ensure nil
  :bind (:map vc-annotate-mode-map
          ("8" . my:open-pr-at-line))
  :config
  (defun my:open-pr-at-line ()
    "Open Pull Request URL at the line from git blame output."
    (interactive)
    (let* ((rev-at-line (vc-annotate-extract-revision-at-line))
            (rev (car rev-at-line)))
      (shell-command (concat "git hub open " rev)))))

(use-package magit
  :ensure t
  :bind (("C-x v s" . magit-status)
          ("C-x v =" . my:magit-diff-unstaged)
          ("C-x v g" . my:magit-blame-toggle)
          :map magit-blame-mode-map
          ("8" . my:open-pr-at-line-magit))
  :hook
  (git-commit-setup . git-commit-turn-on-autofill)
  (git-commit-setup . git-commit-turn-on-flyspell)
  :custom
  ;; 50/72 rules
  (git-commit-summary-max-length 50)
  ;; blame
  (magit-blame-read-only t)
  (magit-blame-styles '((margin
                          (margin-format . ("%.8H %.10C %.12a"))
                          (margin-width . 33)
                          (margin-face . magit-blame-margin)
                          (margin-body-face . magit-blame-dimmed))))
  :config
  (defun my:magit-diff-unstaged ()
    "Show unstaged changes in magit without prompting."
    (interactive)
    (magit-diff-unstaged))

  (defun my:magit-blame-toggle ()
    "Toggle magit blame mode like vc-annotate."
    (interactive)
    (if (bound-and-true-p magit-blame-mode)
      (magit-blame-quit)
      (magit-blame-addition "HEAD")))

  (defun my:open-pr-at-line-magit ()
    "Open Pull Request URL at the line from magit blame output."
    (interactive)
    (when (derived-mode-p 'magit-blame-mode)
      (let ((rev (magit-blame-chunk-get :hash)))
        (when rev
          (shell-command (concat "git hub open " rev)))))))

(use-package browse-at-remote
  :ensure t
  :bind (:map vc-prefix-map
          ("w" . browse-at-remote))
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil))

(use-package git-modes
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package eglot
  :ensure t
  :hook
  (ruby-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (java-mode . eglot-ensure)
  (terraform-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(ruby-ts-mode "ruby-lsp")))

(use-package flymake
  :ensure nil
  :hook
  (prog-mode . flymake-mode))

(use-package posframe
  :ensure t)

(use-package flymake-posframe
  :vc (:url "https://github.com/Ladicle/flymake-posframe.git" :rev :newest)
  :hook
  (flymake-mode . flymake-posframe-mode)
  :custom-face
  (flymake-posframe-face ((t (:foreground "#5fafd7" :background "#292929")))))

(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-default-project nil)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package dash-at-point
  :ensure t
  :bind (("C-c ." . dash-at-point)
          ("C-c C-." . dash-at-point-with-docset)))

(use-package ruby-ts-mode
  :ensure nil
  :mode "Dangerfile"
  :interpreter "ruby"
  :custom
  (ruby-insert-encoding-magic-comment nil)
  :init
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
  :config
  ;; imenu for schema.rb
  (defun rails-schema-imenu-create-index ()
    "Create an imenu index for Ruby on Rails schema.rb."
    (let ((index-alist '())
           (tables '())
           (foreign-keys '())
           (table-regex "^\\s-*create_table\\s-+\"\\([^\"]+\\)\"")
           (foreign-key-regex "^\\s-*add_foreign_key\\s-+\"\\([^\"]+\\)\",\\s-+\"\\([^\"]+\\)\""))
      (goto-char (point-min))
      (while (re-search-forward table-regex nil t)
        (let ((table-name (match-string 1))
               (table-pos (match-beginning 0)))
          (push (cons table-name table-pos) tables)))
      (goto-char (point-min))
      (while (re-search-forward foreign-key-regex nil t)
        (let ((from-table (match-string 1))
               (to-table (match-string 2))
               (foreign-key-pos (match-beginning 0)))
          (push (cons (format "%s -> %s" from-table to-table) foreign-key-pos) foreign-keys)))
      (setq index-alist (append index-alist
                          (list (cons "Tables" (nreverse tables))
                            (cons "Foreign Keys" (nreverse foreign-keys)))))
      index-alist))
  (defun rails-schema-imenu-setup ()
    "Setup imenu for Ruby on Rails schema.rb."
    (setq-local imenu-create-index-function 'rails-schema-imenu-create-index))
  (add-hook 'ruby-ts-mode-hook
    (lambda ()
      (when (and (buffer-file-name)
              (string-match-p "db/schema\\.rb\\'" (buffer-file-name)))
        (rails-schema-imenu-setup)))))

(use-package ruby-end
  :ensure t
  :hook
  (ruby-ts-mode . ruby-end-mode))

(use-package rspec-mode
  :ensure t
  :custom
  (rspec-use-relative-path t)
  (rspec-docker-file-name "compose.yaml")
  ;; Most projects use Compose and have a Rails server running during development
  (rspec-docker-command "docker compose exec")
  ;; Use binstub to run RSpec to use Spring for faster startup
  (rspec-use-bundler-when-possible nil)
  (rspec-spec-command "bin/rspec")
  :config
  (rspec-install-snippets))

(use-package rbenv
  :ensure t
  :custom
  (rbenv-show-active-ruby-in-modeline nil)
  :config
  (global-rbenv-mode))

(use-package projectile-rails
  :ensure t
  :bind-keymap
  ("C-c r" . projectile-rails-command-map)
  :config
  (projectile-rails-global-mode))

(use-package slim-mode
  :vc (:url "https://github.com/slim-template/emacs-slim" :rev :newest :branch "master"))

(use-package cperl-mode
  :ensure nil
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
  :custom-face
  (cperl-array-face ((t (:foreground "#87afd7" :background unspecified :slant normal :weight normal))))
  (cperl-hash-face ((t (:foreground "#87afd7" :background unspecified :slant normal :weight normal))))
  (cperl-nonoverridable-face ((t (:foreground "#d7d700"))))
  :config
  (defalias 'perl-mode 'cperl-mode)

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

(use-package js-mode
  :ensure nil
  :interpreter "node"
  :init
  (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
  :custom
  (js-indent-level 2))

(use-package json-ts-mode
  :ensure nil)

(use-package typescript-ts-mode
  :ensure nil)

(use-package prettier-js
  :ensure t
  :hook
  (js-ts-mode . prettier-js)
  (typescript-ts-mode . prettier-js)
  (tsx-ts-mode . prettier-js))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.erb\\'" "\\.tt\\'" "\\.tx\\'")
  :custom
  (web-mode-attr-indent-offset nil)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-style-padding 2)
  (web-mode-script-padding 2)
  (web-mode-engines-alist '(("template-toolkit" . "\\.tt\\'")
                             ("template-toolkit" . "\\.tx\\'"))))

(use-package css-mode
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
  :custom
  (css-indent-offset 2))

(defun my:escape-html-region (start end)
  "Escape HTML unsafe characters in the region between START and END."
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
  (terraform-mode . terraform-format-on-save-mode))

(use-package yaml-ts-mode
  :ensure nil
  :bind (:map yaml-ts-mode-map
          ("C-m" . newline-and-indent)))

(use-package dockerfile-mode
  :ensure t)

(use-package conf-mode
  :ensure nil
  :mode ("\\.env\\'" . conf-unix-mode))

(use-package org
  :ensure nil
  :bind (("C-x m" . org-capture))
  :custom
  (org-directory "~/Dropbox/org/")
  (org-default-notes-file (concat org-directory "notes.org"))
  (org-startup-truncated nil)
  (org-startup-folded nil)
  (org-return-follows-link t)
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

(use-package groovy-mode)

(use-package vterm
  :ensure t
  :bind (("C-c t" . vterm)
          ("C-c T" . vterm-other-window)
          :map vterm-mode-map
          ("C-l" . my:vterm-send-c-l)
          ("C-u" . my:vterm-send-c-u))
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm %s")
  :config
  ;; Display vterm buffer at bottom with 30% height, respecting current buffer
  (add-to-list 'display-buffer-alist
    '("^\\*vterm"
       (display-buffer-below-selected)
       (window-height . 0.3)))

  (defun my:vterm-send-c-l ()
    "Send C-l directly to terminal for zsh clear-screen behavior."
    (interactive)
    (vterm-send-key "l" nil nil t))

  (defun my:vterm-send-c-u ()
    "Send C-u directly to terminal for zsh kill-whole-line behavior."
    (interactive)
    (vterm-send-key "u" nil nil t))

  (add-hook 'vterm-mode-hook
    (lambda ()
      ;; JuliaMono has excellent Unicode symbols support and Claude Code uses its thinking icons
      (face-remap-add-relative 'default :family "JuliaMono" :weight 'regular)))

  ;; Delete vterm buffer and window when the process is killed
  (add-hook 'vterm-exit-functions
    (lambda (buffer event)
      (when-let ((window (get-buffer-window buffer)))
        (delete-window window))
      (kill-buffer buffer))))

(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :ensure t
  :bind (:map claude-code-command-map
          ("b" . my:claude-code-buffer))
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  :custom
  ;; setting `(claude-code-term-name "xterm-256color")` doesn't fix color issues
  ;; so set eat-term-name directly
  (eat-term-name "xterm-256color")
  :custom-face
  (claude-code-repl-face ((t (:family "JuliaMono" :weight regular))))
  :config
  (add-to-list 'display-buffer-alist
    '("^\\*claude"
       (display-buffer-in-side-window)
       (side . right)
       (window-width . 0.4)))

  ;; Advice to switch to Claude buffer after toggle or start
  (defun my:claude-code-switch-to-buffer (&rest _)
    "Switch to Claude Code buffer after toggle or start."
    (when-let ((claude-buffer (seq-find (lambda (buf)
                                          (string-match "^\\*claude" (buffer-name buf)))
                                (buffer-list))))
      (when (get-buffer-window claude-buffer)
        (select-window (get-buffer-window claude-buffer)))))

  (advice-add 'claude-code-toggle :after #'my:claude-code-switch-to-buffer)
  (advice-add 'claude-code :after #'my:claude-code-switch-to-buffer)

  (defun my:claude-code-buffer ()
    "Send entire buffer to Claude Code."
    (interactive)
    (claude-code-send-region (point-min) (point-max))))

(eval-and-compile
  ;; load additional config per machine
  (let ((host-local-config (expand-file-name "init-local.el" user-emacs-directory)))
    (when (file-exists-p host-local-config)
      (load host-local-config)))

  (when (display-graphic-p)
    ;; make frame height to fit the display height
    (add-hook 'window-setup-hook
      (lambda ()
        (let* ((display-height (display-pixel-height))
                (frame-height (floor (/ display-height (frame-char-height)))))
          (set-frame-size (selected-frame) 140 frame-height))))))

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))
