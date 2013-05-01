(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4)
            (define-key c++-mode-map "\C-cc" 'compile)
            ))
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "stroustrup")
            ))
(setq auto-mode-alist
      (append '(("\\.c$" . c++-mode)
                ("\\.cpp$" . c++-mode)
                ("\\.h$" . c++-mode)) auto-mode-alist))
