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

;; make compact vc-annotate display
(defadvice vc-git-annotate-command (around vc-git-annotate-command activate)
  "Suppress relative path of file from git blame output."
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))

;; open Pull Reuqest URL on this line from vc-annotate enter P as same as tig
(with-eval-after-load 'vc-annotate
  (defun vc-annotate-open-pr-at-line ()
    "Open Pull Request URL at the line from git blame output."
    (interactive)
    (let* ((rev-at-line (vc-annotate-extract-revision-at-line))
           (rev (car rev-at-line)))
      (shell-command (concat "git hub open " rev))))
  (define-key vc-annotate-mode-map (kbd "P") 'vc-annotate-open-pr-at-line))

;; https://snarfed.org/emacs-vc-git-tweaks
;;
;; In vc-git and vc-dir for git buffers, make (C-x v) a run git add, u run git
;; reset, and r run git reset and checkout from head.
(defun my-vc-git-command (verb fn)
  (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset nil t)))
         (backend (car fileset-arg))
         (files (nth 1 fileset-arg)))
    (if (eq backend 'Git)
        (progn (funcall fn files)
               (message (concat verb " " (number-to-string (length files))
                                " file(s).")))
      (message "Not in a vc git buffer."))))

(defun my-vc-git-add (&optional revision vc-fileset comment)
  (interactive "P")
  (my-vc-git-command "Staged" 'vc-git-register))

(defun my-vc-git-reset (&optional revision vc-fileset comment)
  (interactive "P")
  (my-vc-git-command "Unstaged"
                     (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))

(with-eval-after-load 'vc-git
  (define-key vc-prefix-map [(r)] 'vc-revert-buffer)
  (define-key vc-prefix-map [(a)] 'my-vc-git-add)
  (define-key vc-prefix-map [(u)] 'my-vc-git-reset))
(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map [(r)] 'vc-revert-buffer)
  (define-key vc-dir-mode-map [(a)] 'my-vc-git-add)
  (define-key vc-dir-mode-map [(u)] 'my-vc-git-reset)
  ;; hide up to date files after refreshing in vc-dir
  (define-key vc-dir-mode-map [(g)]
    (lambda () (interactive) (vc-dir-refresh) (vc-dir-hide-up-to-date))))

;; flycheck
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(add-hook 'before-save-hook #'gofmt-before-save)

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
