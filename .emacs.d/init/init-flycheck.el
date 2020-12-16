(use-package flycheck
  :pin melpa
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode t)
  :custom-face
  (flycheck-warning ((t (:underline t :weight normal :slant italic))))
  (flycheck-error   ((t (:underline t :weight bold :slant italic)))))

(use-package flycheck-popup-tip
  :config
  (flycheck-popup-tip-mode)
  :custom-face
  (popup-tip-face ((t (:foreground "#5fafd7" :background "#303030")))))
