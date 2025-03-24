(eval-and-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

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
         ("C-M-r" . isearch-backward)
         ("C-M-s" . isearch-forward)
         ("C-c ]" . align-regexp)
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
  (ns-command-modifier 'meta)  ;; make Command ⌘ to Meta
  (ns-option-modifier 'super)  ;; make Option ⌥ to Super
  :init
  (setq custom-file (expand-file-name "custom.el" temporary-file-directory))
  :config
  (menu-bar-mode -1)
  (blink-cursor-mode 0)

  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)

  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

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

  (when (display-graphic-p)
    ;; make frame height to fit the display height
    (add-hook 'window-setup-hook
              (lambda ()
                (let* ((display-height (display-pixel-height))
                       (frame-height (floor (/ display-height (frame-char-height)))))
                  (set-frame-size (selected-frame) 140 frame-height))))
    ;; font
    (set-face-attribute 'default nil
                      :family "Source Han Code JP"
                      :height 110
                      :weight 'medium)
    (set-fontset-font t 'japanese-jisx0208
                      (font-spec :family "Source Han Code JP" :weight 'medium)))

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
    (let* ((fonts-dir (expand-file-name "~/Library/Fonts"))
           (font-file (expand-file-name "NFM.ttf" fonts-dir)))
      (unless (file-exists-p font-file)
        (nerd-icons-install-fonts t)))))

(use-package nerd-icons-completion
  :ensure t
  :hook (after-init . nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-env-version nil))

(use-package whitespace
  :ensure nil
  :custom
  (show-trailing-whitespace t)
  (whitespace-style '(face tabs tab-mark trailing))
  (whitespace-global-modes '(not dired-mode go-mode))
  :config
  (global-whitespace-mode t))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode 1))

(use-package rainbow-mode
  :ensure t
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package tramp
  :ensure nil
  :custom
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

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("e" . wdired-change-to-wdired-mode)
              ("RET" . dired-find-alternate-file)
              ("M-o" . my:dired-open-file-by-open))
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
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

(use-package yasnippet-snippets
  :ensure t)

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map
              ("TAB" . corfu-complete))
  :custom
  (corfu-auto t)
  (tab-always-indent 'complete)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  :config
  (corfu-popupinfo-mode 1)
  ;; sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package cape
  :ensure t
  :bind ("C-o" . cape-dabbrev)
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword))

(use-package company
  :disabled t
  :ensure t
  :bind (("C-o" . company-dabbrev)
         ;; Bind TAB to copilot for now. Remove this when it's stable.
         ;; ("TAB" . company-indent-or-complete-common)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-filter-candidates)
         ("TAB" . company-complete-common-or-cycle)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (global-company-mode t)
  ;; disable inline preview for coexistence with copilot
  (delq 'company-preview-if-just-one-frontend company-frontends)
  :custom
  (company-idle-delay 0)
  (company-auto-expand t)
  (company-minimum-prefix-length 3)
  (company-dabbrev-minimum-length 3)
  ;; go to top at the next of last candidates
  (company-selection-wrap-around t)
  (company-backends '(company-capf company-semantic company-dabbrev-code company-dabbrev company-keywords company-yasnippet)))

(use-package company-prescient
  :disabled t
  :ensure t)

(use-package company-quickhelp
  :disabled t
  :ensure t
  :config
  (company-quickhelp-mode))

(use-package projectile
  :ensure t
  :bind (("C-x f" . projectile-find-file-dwim)
         ("C-x p" . projectile-switch-project))
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
         ("C-c f" . consult-find)
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

(use-package dash-at-point
  :ensure t
  :bind (("C-c ." . dash-at-point)
         ("C-c C-." . dash-at-point-with-docset)))

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
  :hook (emacs-lisp-mode . my:disable-flycheck-in-init)
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

(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-default-project nil)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

(use-package flycheck-popup-tip
  :ensure t
  :config
  (flycheck-popup-tip-mode)
  :custom-face
  (popup-tip-face ((t (:foreground "#5fafd7" :background "#292929")))))

(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-make-backup-files t)
  ;; Limit vc backends to Git only to improve performance of tramp:)
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (vc-handled-backends '(Git)))

(require 'vc-git)
(require 'vc-dir)
(require 'vc-annotate)

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

(use-package magit
  :ensure t
  :bind (("C-x v s" . magit-status)
         ("C-x v =" . my:magit-diff-unstaged))
  :custom
  ;; 50/72 rules
  (git-commit-summary-max-length 50)
  (git-commit-fill-column 72)
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
    (magit-diff-unstaged)))

(use-package browse-at-remote
  :ensure t
  :bind (:map vc-prefix-map
              ("w" . browse-at-remote)))

(use-package git-modes
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package eglot
  :ensure t
  :hook ((ruby-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (java-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(ruby-mode "ruby-lsp")))

;; (use-package lsp-mode
;;   :ensure t
;;   :commands
;;   (lsp lsp-deferred)
;;   :bind-keymap
;;   ("C-c l" . lsp-command-map)
;;   :custom
;;   (lsp-headerline-breadcrumb-enable nil)
;;   :config
;;   ;; rubocop-ls is highest priority (-1) in ruby language servers, however
;;   ;; I want use ruby-lsp because it uses rubocop as formatter and provides
;;   ;; other features.
;;   (add-to-list 'lsp-disabled-clients 'rubocop-ls)
;;   ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
;;   (setq gc-cons-threshold 100000000)
;;   ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
;;   (setq read-process-output-max (* 1024 1024)))

;; (use-package lsp-docker)

;; (use-package consult-lsp
;;   :ensure t)

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :ensure t
  :after company
  :hook (prog-mode . copilot-mode)
  :bind (("TAB" . my:copilot-tab)
         :map copilot-completion-map
         ("C-g" . copilot-clear-overlay)
         ("C-n" . copilot-next-completion)
         ("C-p" . copilot-previous-completion)
         ("TAB" . copilot-accept-completion))
  :config
  (defun my:copilot-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (company-indent-or-complete-common nil))))

(use-package ruby-mode
  :ensure nil
  :mode "Dangerfile"
  :interpreter "ruby"
  ;; :hook
  ;; (ruby-mode . lsp-deferred)
  :custom
  (ruby-insert-encoding-magic-comment nil)
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
    ;; disable lsp--imenu-create-index provided by lsp-mode
    (when (fboundp 'lsp-mode)
      (lsp-mode -1))
    (setq-local imenu-create-index-function 'rails-schema-imenu-create-index))
  (add-hook 'ruby-mode-hook
            (lambda ()
              (when (and (buffer-file-name)
                         (string-match-p "db/schema\\.rb\\'" (buffer-file-name)))
                (rails-schema-imenu-setup)))))

(use-package ruby-end
  :ensure t
  :hook (ruby-mode . ruby-end-mode))

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

(use-package inf-ruby
  :ensure t
  :hook ((ruby-mode . inf-ruby-minor-mode)
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
  (cperl-font-lock t)
  :custom-face
  (cperl-array-face ((t (:foreground "#87afd7" :background unspecified :slant normal :weight normal))))
  (cperl-hash-face ((t (:foreground "#87afd7" :background unspecified :slant normal :weight normal))))
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

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :custom
  (js2-basic-offset 2))

(use-package json-mode
  :ensure t)

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :custom
  (typescript-indent-level 2))

(use-package prettier-js
  :ensure t
  :hook ((js2-mode . prettier-js)
         (typescript-mode . prettier-js)))

(use-package web-mode
  :ensure t
  :mode ("\\.jsx\\'" "\\.tsx\\'" "\\.html\\'" "\\.tt\\'" "\\.tx\\'")
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
  :hook ((terraform-mode . terraform-format-on-save-mode)
         (terraform-mode . lsp-deferred)))

(use-package yaml-mode
  :ensure t
  :bind (:map yaml-mode-map
         ("C-m" . newline-and-indent)))

(use-package docker-compose-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package org
  :ensure nil
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

;; (use-package lsp-java
;;   :hook (java-mode . lsp-deferred))

(use-package groovy-mode)

(eval-and-compile
  ;; load additional config per machine
  (let ((host-local-config (expand-file-name "init-local.el" user-emacs-directory)))
    (when (file-exists-p host-local-config)
      (load host-local-config)))
  ;; to know that GC is running
  (setq garbage-collection-messages t)
  ;; start server for emacsclient
  (when (display-graphic-p)
    (require 'server)
    (unless (server-running-p)
      (server-start))))
