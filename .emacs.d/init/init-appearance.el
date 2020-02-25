(menu-bar-mode 0)
(tool-bar-mode 0)
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

;; UI colors
(set-face-attribute 'default nil
                    :foreground "#e4e4e4"
                    :background "#080808")
(set-face-attribute 'highlight nil
                    :foreground "#080808"
                    :background "green")
(set-face-attribute 'region nil
                    :foreground "#e4e4e4"
                    :background "gray22")
(set-face-attribute 'minibuffer-prompt nil
                    :foreground "yellow")
(set-face-attribute 'mode-line nil
                    :foreground "#c6c6c6"
                    :background "#444444")
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#6c6c6c"
                    :background "gray16")
(set-face-attribute 'mode-line-buffer-id nil
                    :foreground "#ff8700"
                    :weight 'normal)
(set-face-attribute 'header-line nil
                    :inherit 'mode-line
                    :weight 'bold
                    :slant 'italic
                    :underline t)

;; Coding syntax colors
(set-face-attribute 'font-lock-comment-face nil
                    :foreground "gray52"
                    :slant 'italic)
(set-face-attribute 'font-lock-string-face nil
                    :foreground "#afd787")
(set-face-attribute 'font-lock-keyword-face nil
                    :foreground "#ffaf00")
(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "#afafaf")
(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground "#87afd7")
(set-face-attribute 'font-lock-constant-face nil
                    :foreground "#d75f5f")
(set-face-attribute 'font-lock-type-face nil
                    :foreground "#af87ff")
(set-face-attribute 'font-lock-warning-face nil
                    :foreground "#af0000")
(set-face-attribute 'font-lock-builtin-face nil
                    :foreground "#d787d7")

;; highlight hard tab, trailing spaces and double width space
(use-package whitespace
  :config
  (setq show-trailing-whitespace t)
  (setq whitespace-style '(face tabs tab-mark trailing))
  (global-whitespace-mode t)
  :custom-face
  (whitespace-trailing ((t (:foreground "gray90" :background "gray32"))))
  (whitespace-tab ((t (:foreground "gray40" :background nil)))))

(use-package rainbow-mode)
