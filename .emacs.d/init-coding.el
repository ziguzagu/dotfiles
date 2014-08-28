;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8 -*-

;; make compact vc-annotate display
(defadvice vc-git-annotate-command (around vc-git-annotate-command activate)
  "suppress relative path of file from git blame output"
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))

; (add-hook 'after-init-hook #'global-flycheck-mode)
