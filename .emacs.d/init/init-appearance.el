;;; init-appearance.el --- configure appearance
;;; Commentary:
;;; Code:

;; hide startup message
(setq inhibit-startup-message t)

;; hide menu bar, toolbar and scroll bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; stop cursor blinking
(blink-cursor-mode 0)

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

;; common syntax colors
(set-face-attribute 'font-lock-comment-face nil
                    :foreground "gray52" :slant 'italic)
(set-face-foreground 'font-lock-string-face  "darkolivegreen3")
(set-face-foreground 'font-lock-keyword-face "goldenrod2")
(set-face-foreground 'font-lock-function-name-face "azure3")
(set-face-foreground 'font-lock-variable-name-face "#81c4c5")
(set-face-foreground 'font-lock-constant-face "indianred")
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

;; modeline faces
(make-face 'mode-line-vc-mode)
(custom-set-faces
 '(mode-line           ((t (:foreground "#bcbcbc" :background "#444444" :box nil))))
 '(mode-line-inactive  ((t (:foreground "gray42" :background "gray16" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#ff8700" :weight normal))))
 '(mode-line-vc-mode   ((t (:foreground "#5fafff" :weight normal)))))
;; modeline content
(setq-default mode-line-format
              (list "-"
                    'mode-line-mule-info
                    'mode-line-modified
                    " "
                    'mode-line-buffer-identification
                    '(:eval (concat (propertize " %c:%l(%p)")))
                    '(:propertize (:eval vc-mode) face mode-line-vc-mode)
                    " "
                    'mode-line-modes
                    "-%-"))

;; minibuffer color
(set-face-foreground 'minibuffer-prompt "yellow")

;; settings for running in GUI world
(when window-system
  (setq-default line-spacing 0)
  (set-face-attribute 'default nil :family "Source Han Code JP" :height 120))

;; diff-mode colors
(custom-set-faces
 '(diff-file-header ((t (:background "gray32" :foreground "orange"))))
 '(diff-header      ((t (:background "gray32" :foreground "gray70"))))
 '(diff-context     ((t (:inherit shadow :foreground "gray90"))))
 '(diff-changed     ((t (:background "yellow" :foreground "black"))))
 '(diff-added       ((t (:background "darkolivegreen3" :foreground "black"))))
 '(diff-removed     ((t (:background "tomato" :foreground "black")))))

;; highlight hard tab, trailing spaces and double width space
(use-package whitespace
  :diminish (global-whitespace-mode whitespace-mode)
  :config
  (setq show-trailing-whitespace t)
  (setq whitespace-style '(face tabs tab-mark trailing))
  (custom-set-faces
   '(whitespace-trailing ((t (:foreground "gray90" :background "gray32" :inverse-video nil))))
   '(whitespace-tab      ((t (:foreground "gray70" :background nil :inverse-video nil)))))
  (global-whitespace-mode t))

;;; init-appearance.el ends here
