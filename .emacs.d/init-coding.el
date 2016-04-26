;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8 -*-

;; make compact vc-annotate display
(defadvice vc-git-annotate-command (around vc-git-annotate-command activate)
  "suppress relative path of file from git blame output"
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))

;; enable flycheck globally
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(set-face-attribute 'flycheck-warning nil
                    :foreground "gray1"
                    :background "yellow"
                    :weight 'normal)
(set-face-attribute 'flycheck-error nil
                    :foreground "white"
                    :background "red1"
                    :weight 'normal)

;; improve flycheck error display with popup
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-mysql-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
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
                                :foreground "color-75")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'before-save-hook #'gofmt-before-save)
