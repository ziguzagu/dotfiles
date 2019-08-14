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

;; modeline faces
(make-face 'mode-line-vc-mode)
(make-face 'fc-error-face)
(make-face 'fc-warning-face)
(make-face 'fc-info-face)
(custom-set-faces
 '(mode-line           ((t (:foreground "#bcbcbc" :background "#444444" :box nil))))
 '(mode-line-inactive  ((t (:foreground "gray42" :background "gray16" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#ff8700" :weight normal))))
 '(mode-line-vc-mode   ((t (:foreground "#5fafff" :weight normal))))
 '(fc-error-face       ((t (:foreground "#fb4933" :weight normal))))
 '(fc-warning-face     ((t (:foreground "#fabd2f" :weight normal))))
 '(fc-info-face        ((t (:foreground "#83a598" :weight normal)))))
;; get rid of 'Git:' from vc-mode
(defun vc-branch ()
  (let ((backend (vc-backend buffer-file-name)))
    (substring vc-mode 5)))
;; customize flycheck modeline display
(defun my-checker (state)
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "?"))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running) (format "•%s" err))))
(defun mode-line-chcker ()
  (when (and (bound-and-true-p flycheck-mode)
             (or flycheck-current-errors
                 (eq 'running flycheck-last-status-change)))
    (cl-loop for state in '(error warning info)
             as ret = (my-checker state)
             when ret
             concat (propertize
                     ret
                     'face (intern (format "fc-%S-face" state))))))
;; modeline content
(setq-default mode-line-format
              (list " "
                    'mode-line-mule-info
                    'mode-line-modified
                    " "
                    'mode-line-buffer-identification
                    " %c:%l(%p)"
                    '(vc-mode
                     ((:propertize " " face mode-line-vc-mode)
                      (:propertize (:eval (vc-branch)) face mode-line-vc-mode)))
                    " "
                    'mode-name
                    " "
                    '(:eval (mode-line-chcker))))

;; minibuffer color
(set-face-foreground 'minibuffer-prompt "yellow")

;; settings for running in GUI world
(when window-system
  ;; powerline
  (use-package spaceline-config
    :ensure spaceline
    :config
    (setq powerline-default-separator 'wave)
    (setq ns-use-srgb-colorspace nil) ;; fix wrong separator color
    (spaceline-helm-mode))
  (use-package spaceline-all-the-icons
    :after spaceline
    :config
    (setq spaceline-all-the-icons-separator-scale 1.7)
    (setq spaceline-all-the-icons-separator-type 'wave)
    (spaceline-all-the-icons-theme))
  ;; tweak font
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
