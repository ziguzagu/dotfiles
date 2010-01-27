;;;; -*- mode: lisp-interaction; syntax: elisp; coding: iso-2022-7bit -*-

(provide 'init-color)

(require 'font-lock)
(global-font-lock-mode t)
;; basics
(set-face-foreground 'default "white")
(set-face-background 'default "black")
(set-cursor-color "green")
;; common syntax
(set-face-foreground 'font-lock-comment-face "green")
(set-face-foreground 'font-lock-string-face  "red")
(set-face-foreground 'font-lock-keyword-face "magenta")
(set-face-foreground 'font-lock-function-name-face "white")
(set-face-foreground 'font-lock-variable-name-face "white")
(set-face-foreground 'font-lock-type-face "cyan")
(set-face-background 'font-lock-warning-face "red")
(set-face-foreground 'font-lock-warning-face "white")
(set-face-foreground 'font-lock-builtin-face "yellow")
;; highlight
(set-face-background 'highlight "yellow")
(set-face-foreground 'highlight "black")
;; region
(set-face-background 'region "gray")
(set-face-foreground 'region "black")
;; modeline
(set-face-foreground 'modeline "black")
(set-face-background 'modeline "cyan")
(set-face-foreground 'mode-line-buffer-id "white")
(set-face-foreground 'mode-line-inactive "black")
(set-face-background 'mode-line-inactive "brightblack")
;; mini buffer
(set-face-foreground 'minibuffer-prompt "yellow")
