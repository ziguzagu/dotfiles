;;;; -*- mode: lisp-interaction; syntax: elisp; coding: iso-2022-7bit -*-
;;
;; jaspace.el: http://homepage3.nifty.com/satomii/software/jaspace.el
;;

(provide 'init-space-color)

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
