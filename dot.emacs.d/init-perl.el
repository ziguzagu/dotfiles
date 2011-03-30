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

(global-set-key (kbd "\C-c .") 'cperl-perldoc)

(add-hook 'cperl-mode-hook
          (lambda ()
            (copy-face 'font-lock-variable-name-face 'cperl-array-face)
            (copy-face 'font-lock-variable-name-face 'cperl-hash-face)
            (set-face-foreground 'cperl-nonoverridable-face "yellow")
            (define-key cperl-mode-map "\M-." 'cperl-perldoc-at-point)))

(setq auto-mode-alist
      (append '(("\\.cgi$" . cperl-mode)
                ("\\.p[hlm]$" . cperl-mode)
                ("\\.psgi$" . cperl-mode)
                ("\\.t$" . cperl-mode)) auto-mode-alist))

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
            (define-key cperl-mode-map "\C-ct" 'perltidy-region)
            (define-key cperl-mode-map "\C-cT" 'perltidy-buffer)))

;; perl-completion
(add-hook 'cperl-mode-hook
          (lambda ()
            (require 'perl-completion)
            (perl-completion-mode t)))
