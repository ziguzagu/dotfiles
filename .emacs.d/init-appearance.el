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
(set-face-foreground 'font-lock-comment-face "gray46")
(set-face-foreground 'font-lock-string-face  "darkolivegreen3")
(set-face-foreground 'font-lock-keyword-face "orchid2")
(set-face-foreground 'font-lock-function-name-face "white")
(set-face-foreground 'font-lock-variable-name-face "turquoise3")
(set-face-foreground 'font-lock-constant-face "tomato")
(set-face-foreground 'font-lock-type-face "mediumpurple1")
(set-face-background 'font-lock-warning-face "red")
(set-face-foreground 'font-lock-warning-face "white")
(set-face-foreground 'font-lock-builtin-face "yellow")

;; highlight colors
(set-face-background 'highlight "green")
(set-face-foreground 'highlight "black")

;; region colors
(set-face-background 'region "gray22")
(set-face-foreground 'region "white")

;; modeline colors
(set-face-foreground 'mode-line "gray70")
(set-face-background 'mode-line "gray32")
(set-face-foreground 'mode-line-inactive "gray32")
(set-face-background 'mode-line-inactive "gray16")
(set-face-foreground 'mode-line-buffer-id "orange")
;; modeline content
(setq-default mode-line-format
              (list "-"
                    'mode-line-mule-info
                    'mode-line-modified
                    " "
                    'mode-line-buffer-identification
                    '(:eval (concat (propertize "  %p %l:%c ")))
                    'vc-mode
                    "  "
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

;; setup color of zenkaku space, leading/trailing space and tab
;;  * http://homepage3.nifty.com/satomii/software/jaspace.el
(when (require 'jaspace nil t)
  (when (boundp 'jaspace-modes)
    (setq jaspace-modes (append jaspace-modes
                                (list 'cperl-mode
                                      'js2-mode
                                      'css-mode
                                      'html-helper-mode
                                      'yaml-mode
                                      'c-mode
                                      'c++-mode
                                      'text-mode
                                      'default-generic-mode
                                      'fundamental-mode))))
  (when (boundp 'jaspace-alternate-jaspace-string)
    (setq jaspace-alternate-jaspace-string "□"))
  (when (boundp 'jaspace-highlight-tabs)
    (setq jaspace-highlight-tabs ?^))
  (add-hook 'jaspace-mode-off-hook
            (lambda()
              (when (boundp 'show-trailing-whitespace)
                (setq show-trailing-whitespace nil))))
  (add-hook 'jaspace-mode-hook
            (lambda()
              (progn
                (when (boundp 'show-trailing-whitespace)
                  (setq show-trailing-whitespace t))
                (face-spec-set 'jaspace-highlight-jaspace-face
                               '((((class color) (background light))
                                  (:foreground "blue"))
                                 (t (:foreground "green"))))
                (face-spec-set 'jaspace-highlight-tab-face
                               '((((class color) (background light))
                                  (:foreground "red"
                                               :background "unspecified"
                                               :strike-through nil
                                               :underline t))
                                 (t (:foreground "magenta"
                                                 :background "unspecified"
                                                 :strike-through nil
                                                 :underline t))))
                (face-spec-set 'trailing-whitespace
                               '((((class color) (background light))
                                  (:foreground "red"
                                               :background "unspecified"
                                               :strike-through nil
                                               :underline t))
                                 (t (:foreground "magenta"
                                                 :background "unspecified"
                                                 :strike-through nil
                                                 :underline t))))))))