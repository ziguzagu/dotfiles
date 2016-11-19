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
                    'mode-name
                    'minor-mode-alist
                    "-%-"))
;; rich modeline
(when window-system
  (use-package all-the-icons)
  (defun custom-modeline-modified-icon ()
    (let* ((config-alist
            '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
              ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
              ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
           (result (cdr (assoc (format-mode-line "%*") config-alist))))
      (propertize (apply (cadr result) (cddr result))
                  'face `(:family ,(funcall (car result))))))
  (defun custom-modeline-mode-icon ()
    (format "%s"
            (propertize (all-the-icons-icon-for-file buffer-file-name)
                        'help-echo (format "Mode: `%s`" mode-name)
                        'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer))
                        'display '(raise -0.1))))
  (defun -custom-modeline-vc-github ()
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                   'display '(raise -0.1))
       (propertize (format " %s" branch) 'face `(:height 0.9)))))
  (defun custom-modeline-vc-icon ()
    (when vc-mode
      (cond
       ((string-match "Git[:-]" vc-mode) (-custom-modeline-vc-github))
       (t (format "%s" vc-mode)))))
  (setq-default mode-line-format
                (list "  "
                      '(:eval (custom-modeline-modified-icon))
                      "  "
                      '(:eval (custom-modeline-mode-icon))
                      "  "
                      'mode-line-buffer-identification
                      "  "
                      '(:eval (custom-modeline-vc-icon))
                      "  "
                      '(:eval (format-mode-line "%4l:%3c"))
                      "  "
                      'minor-mode-alist)))

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
