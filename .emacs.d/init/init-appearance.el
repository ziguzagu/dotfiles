;;; init-appearance.el --- configure appearance
;;; Commentary:
;;; Code:

;; hide startup message
(setq inhibit-startup-message t)

;; hide menu bar, toolbar and scroll bar
(menu-bar-mode 0)
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

;; stop cursor blinking
(blink-cursor-mode 0)

;; turn off both beep and visual bell
(setq ring-bell-function 'ignore)

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
(set-face-foreground 'default "#e4e4e4")
(set-face-background 'default "#000000")

;; common syntax colors
(set-face-attribute 'font-lock-comment-face nil
                    :foreground "gray52" :slant 'italic)
(set-face-foreground 'font-lock-string-face  "#afd787")
(set-face-foreground 'font-lock-keyword-face "#ffaf00")
(set-face-foreground 'font-lock-function-name-face "#afafaf")
(set-face-foreground 'font-lock-variable-name-face "#87afd7")
(set-face-foreground 'font-lock-constant-face "#d75f5f")
(set-face-foreground 'font-lock-type-face "#af87ff")
(set-face-background 'font-lock-warning-face "red")
(set-face-foreground 'font-lock-warning-face "white")
(set-face-foreground 'font-lock-builtin-face "#d787d7")

;; highlight colors
(set-face-background 'highlight "green")
(set-face-foreground 'highlight "black")

;; region colors
(set-face-background 'region "gray22")
(set-face-foreground 'region "white")

;; minibuffer color
(set-face-foreground 'minibuffer-prompt "yellow")

(set-face-attribute 'header-line nil
                    :inherit 'mode-line
                    :weight 'bold
                    :slant 'italic
                    :underline t)

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
  (set-face-attribute 'whitespace-trailing nil
                      :foreground "gray90"
                      :background "gray32"
                      :inverse-video nil)
  (set-face-attribute 'whitespace-tab nil
                      :foreground "gray70"
                      :background nil
                      :inverse-video nil)
  (global-whitespace-mode t))

;;; init-appearance.el ends here
