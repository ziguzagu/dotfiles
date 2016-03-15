;;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8 -*-

;; hide startup message
(setq inhibit-startup-message t)

;; hide menu bar
(menu-bar-mode -1)

;; turn off both beep and visual bell
(setq ring-bell-function 'ignore)

;; show line/column numer in modeline
(line-number-mode t)
(column-number-mode t)

;; highlight selected region
(setq-default transient-mark-mode t)

;; fix mini-buffer
(setq resize-mini-windows nil)

;; turn on coloring
(require 'font-lock)
(global-font-lock-mode t)

;; use 256 colors on screen-256color term
(defun terminal-init-screen ()
  "Terminal initialization function for screen-256color."
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))

;; basic colors
(set-face-foreground 'default "white")
(set-face-background 'default "black")
(set-cursor-color "orange")

;; common syntax colors
(set-face-foreground 'font-lock-comment-face "gray52")
(set-face-foreground 'font-lock-string-face  "darkolivegreen3")
(set-face-foreground 'font-lock-keyword-face "goldenrod2")
(set-face-foreground 'font-lock-function-name-face "azure3")
(set-face-foreground 'font-lock-variable-name-face "darkturquoise")
(set-face-foreground 'font-lock-constant-face "firebrick1")
(set-face-foreground 'font-lock-type-face "mediumpurple2")
(set-face-background 'font-lock-warning-face "red")
(set-face-foreground 'font-lock-warning-face "white")
(set-face-foreground 'font-lock-builtin-face "plum3")

;; highlight colors
(set-face-background 'highlight "green")
(set-face-foreground 'highlight "black")

;; region colors
(set-face-background 'region "gray22")
(set-face-foreground 'region "white")

;; modeline colors
(set-face-foreground 'mode-line "gray72")
(set-face-background 'mode-line "gray28")
(set-face-foreground 'mode-line-inactive "gray42")
(set-face-background 'mode-line-inactive "gray16")
(set-face-attribute 'mode-line-buffer-id nil
                    :foreground "color-208" :weight 'normal)
;; custom face for vc-mode in mode-line
(make-face 'mode-line-vc-mode-face)
(set-face-attribute 'mode-line-vc-mode-face nil
                    :foreground "color-75" :weight 'normal)
;; modeline content
(setq-default mode-line-format
              (list "-"
                    'mode-line-mule-info
                    'mode-line-modified
                    " "
                    'mode-line-buffer-identification
                    '(:eval (concat (propertize " %c:%l(%p)")))
                    '(:propertize (:eval vc-mode) face mode-line-vc-mode-face)
                    " "
                    'mode-line-modes
                    "-%-"))

;; minibuffer color
(set-face-foreground 'minibuffer-prompt "yellow")

;; diff-mode colors
(custom-set-faces
 '(diff-file-header ((t (:background "gray32" :foreground "orange"))))
 '(diff-header      ((t (:background "gray32" :foreground "gray70"))))
 '(diff-context     ((t (:inherit shadow :foreground "gray90"))))
 '(diff-changed     ((t (:background "yellow" :foreground "black"))))
 '(diff-added       ((t (:background "darkolivegreen3" :foreground "black"))))
 '(diff-removed     ((t (:background "tomato" :foreground "black")))))

;; highlight hard tab, trailing spaces and double width space
(require 'whitespace)
(global-whitespace-mode t)
(setq show-trailing-whitespace t)
(setq whitespace-style '(tabs tab-mark tailing spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(set-face-background 'trailing-whitespace "steelblue")
(set-face-background 'whitespace-space "steelblue")
(set-face-foreground 'whitespace-tab "steelblue")
(set-face-background 'whitespace-tab "steelblue")
