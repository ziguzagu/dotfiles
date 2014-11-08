;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8 -*-

;; make compact vc-annotate display
(defadvice vc-git-annotate-command (around vc-git-annotate-command activate)
  "suppress relative path of file from git blame output"
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))

;; enable flycheck globally
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; flycheck-pos-tip extension improves flycheck error display with popup
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
