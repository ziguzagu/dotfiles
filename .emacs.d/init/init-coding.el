(use-package diff-mode
  :config
  (set-face-attribute 'diff-file-header nil
                      :foreground "orange"
                      :background "gray32")
  (set-face-attribute 'diff-header nil
                      :foreground "gray70"
                      :background "gray32")
  (set-face-attribute 'diff-context nil
                      :inherit 'shadow
                      :foreground "gray90")
  (set-face-attribute 'diff-changed nil
                      :foreground "black"
                      :background "yellow")
  (set-face-attribute 'diff-added nil
                      :foreground "black"
                      :background "darkolivegreen3")
  (set-face-attribute 'diff-removed nil
                      :foreground "black"
                      :background "tomato"))

(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode t)
  (set-face-attribute 'flycheck-warning nil
                      :underline t
                      :weight 'normal
                      :slant 'italic)
  (set-face-attribute 'flycheck-error nil
                      :underline t
                      :weight 'bold
                      :slant 'italic))
(use-package flycheck-popup-tip
  :config
  (flycheck-popup-tip-mode))

(use-package sql
  :config
  (sql-highlight-mysql-keywords))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (set-face-attribute 'markdown-header-delimiter-face nil
                      :foreground "orange")
  (set-face-attribute 'markdown-header-rule-face nil
                      :foreground "orange")
  (set-face-attribute 'markdown-header-face nil
                      :foreground "orange")
  (set-face-attribute 'markdown-inline-code-face nil
                      :foreground "darkolivegreen3")
  (set-face-attribute 'markdown-pre-face nil
                      :foreground "darkolivegreen3")
  (set-face-attribute 'markdown-language-keyword-face nil
                      :foreground "gray52")
  (set-face-attribute 'markdown-list-face nil
                      :foreground "mediumpurple1" :weight 'bold)
  (set-face-attribute 'markdown-link-face nil
                      :foreground "color-75"))

(use-package sh-script
  :init
  (setq sh-indentation 2)
  (setq sh-basic-offset 2)
  (setq sh-shell-file "/bin/bash"))

(use-package dumb-jump
  :bind (("M-." . dumb-jump-go)
         ("M-," . dumb-jump-back))
  :init
  (setq dumb-jump-selector 'helm)
  (setq dumb-jump-default-project nil))
