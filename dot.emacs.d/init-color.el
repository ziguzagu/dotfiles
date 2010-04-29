;;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8 -*-

(require 'font-lock)
(global-font-lock-mode t)

;; basics
(set-face-foreground 'default "white")
(set-face-background 'default "black")
(set-cursor-color "green")

;; common syntax
(set-face-foreground 'font-lock-comment-face "brightblack")
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
(set-face-background 'region "brightblack")
(set-face-foreground 'region "white")

;; modeline
(set-face-foreground 'modeline "black")
(set-face-background 'modeline "cyan")
(set-face-foreground 'mode-line-buffer-id "white")
(set-face-foreground 'mode-line-inactive "black")
(set-face-background 'mode-line-inactive "brightblack")

;; mini buffer
(set-face-foreground 'minibuffer-prompt "yellow")

;; diff-mode
(custom-set-faces
 '(diff-file-header ((t (:background "brightblack" :foreground "white"))))
 '(diff-header      ((t (:background "brightblack" :foreground "white"))))
 '(diff-context     ((t (:inherit shadow :foreground "white"))))
 '(diff-changed     ((t (:background "yellow" :foreground "black"))))
 '(diff-added       ((t (:background "green" :foreground "black"))))
 '(diff-removed     ((t (:background "brightred" :foreground "black")))))

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

(provide 'init-color)
