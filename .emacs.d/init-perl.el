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

(add-hook 'cperl-mode-hook
          (lambda ()
            (copy-face 'font-lock-variable-name-face 'cperl-array-face)
            (copy-face 'font-lock-variable-name-face 'cperl-hash-face)
            (set-face-foreground 'cperl-nonoverridable-face "yellow")
            (define-key cperl-mode-map "\M-." 'cperl-perldoc-at-point)
            (define-key cperl-mode-map "\C-c ." 'cperl-perldoc)))

(add-to-list 'auto-mode-alist '("\\.cgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.p[hlm]$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.psgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))

;; setup PATH for perlbrew to find correct perldoc, perltidy and some commands installed by perlbrew
;; (load "cl-seq")
;; (mapc (lambda (x) (add-to-list 'exec-path x))
;;       (mapcar (lambda (x) (concat (getenv "HOME") x))
;;               (list "/perl5/perlbrew/bin" "/perl5/perlbrew/perls/current/bin")))
;; (setenv "PATH"
;;         (reduce (lambda (a b) (concatenate 'string a ":" b))
;;                 exec-path))

;; setup PATH and etc form plenv
;; (el-get 'sync 'plenv)
;; (require 'plenv)
;; (plenv-global "5.16.2")

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
