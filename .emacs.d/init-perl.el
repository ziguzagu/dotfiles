;;;; -*- mode: lisp-interaction; syntax: elisp; -*-

(defalias 'perl-mode 'cperl-mode)
(setq cperl-close-paren-offset -4)
(setq cperl-continued-statement-offset 4)
(setq cperl-indent-level 4)
(setq cperl-label-offset -4)
(setq cperl-indent-parens-as-block t)
(setq cperl-tab-always-indent t)
(setq cperl-auto-newline nil)
(setq cperl-electric-linefeed nil)
(setq cperl-autoindent-on-semi t)
(setq cperl-highlight-variables-indiscriminately t)
(setq cperl-font-lock t)

(font-lock-add-keywords 'cperl-mode
                        '(("state" . font-lock-keyword-face)))

(add-hook 'cperl-mode-hook
          (lambda ()
            (copy-face 'font-lock-variable-name-face 'cperl-array-face)
            (copy-face 'font-lock-variable-name-face 'cperl-hash-face)
            (set-face-foreground 'cperl-nonoverridable-face "yellow3")
            (local-set-key (kbd "M-.") 'cperl-perldoc-at-point)
            (local-set-key (kbd "C-c .") 'cperl-perldoc)))

(add-to-list 'auto-mode-alist '("\\.cgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.p[hlm]$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.psgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))
(add-to-list 'auto-mode-alist '("cpanfile" . cperl-mode))

;; plenv
(use-package plenv)

;; ffap with perldoc
(defun ffap-cperl-mode (file)
  (let ((real-file (shell-command-to-string (concat "perldoc -lm " file))))
    (unless (string-match "No module found for " real-file)
      (substring real-file (string-match "/" real-file) -1))))
(add-to-list 'ffap-alist '(cperl-mode . ffap-cperl-mode))

;; perltidy
(defun perltidy-region (beg end)
  (interactive "r")
  (shell-command-on-region beg end "perltidy -q" nil t))
(defun perltidy-buffer (buffer)
  "Run the perltidy formatter on the buffer."
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (perltidy-region (point-min) (point-max))))
(add-hook 'cperl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c t") 'perltidy-region)
            (local-set-key (kbd "C-c T") 'perltidy-buffer)))

;; flycheck with Project::Libs
(flycheck-define-checker perl-project-libs
  "A perl syntax checker with Project::Libs."
  :command ("plenv"
            "exec"
            "perl"
            "-MProject::Libs lib_dirs => [qw(local/lib/perl5 core/lib typepad/lib mars/lib dolphin/lib)]"
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
