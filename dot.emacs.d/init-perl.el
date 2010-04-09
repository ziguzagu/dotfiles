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
            (set-face-foreground 'cperl-nonoverridable-face "cyan")
            (define-key cperl-mode-map "\M-." 'cperl-perldoc-at-point)
            (define-key cperl-mode-map "\C-co" 'cperl-perldoc)))

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

;; snippets
(when (require 'snippet nil t)
  (defvar cperl-mode-abbrev-table nil)
  (define-abbrev-table 'cperl-mode-abbrev-table ())
  (snippet-with-abbrev-table 'cperl-mode-abbrev-table
                             ;; basics
                             ("pfor" . "for my $${var} ($${array}) {\n$>$.\n}")
                             ("psub" . "sub $${name} {\n$>my $self = shift;\n$>$.\n}\n")
                             ("pif" . "if ($${condition}) {\n$>$.\n}\n")
                             ("punless" . "unless ($${condition}) {\n$>$.\n}\n")
                             ;; moose
                             ("phas" . "has '$${name}' => (is => 'rw', isa => '$${type}'$.);\n")
                             ("psubtype" . "subtype '$${type}'\n$>=> as '$${belongs to}'\n$>=> where { $${constraints of construction} };\n")
                             ("pcoerce" . "coerce '$${type}'\n$>=> from '$${from type 1}'\n$>    => via { $${construction code 1} }\n$>$>=> from '$${from type 2}'\n$>    => via { $${construction code 2} }\n$>;\n")))

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

;; perl-completion with auto-complete
;; (add-hook 'cperl-mode-hook
;;           (lambda ()
;;             (require 'perl-completion)
;;             (perl-completion-mode t)
;;             (when (require 'auto-complete nil t)
;;               (auto-complete-mode t)
;;               (make-variable-buffer-local 'ac-sources)
;;               (add-to-list 'ac-sources 'ac-source-perl-completion))))
;; (setq ac-candidate-max 1000)

(provide 'init-perl)
