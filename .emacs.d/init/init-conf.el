;;; init-conf.el --- settings for various middleware's config file modes
;;; Commentary:
;;; Code:

(setq auto-mode-alist
      (append '(("\\.conf$" . conf-mode)
                ("\\.cfg$"  . conf-mode)) auto-mode-alist))

;;;;;; yaml
(use-package yaml-mode
  :bind (:map yaml-mode-map
         ("C-m" . newline-and-indent)))

;;;;;; vcl mode
(autoload 'vcl-mode "vcl-mode" "Major mode for editting varnish config file")
(add-to-list 'auto-mode-alist '("\\.vcl$" . vcl-mode))
(add-to-list 'auto-mode-alist '("\\.vtc$" . vcl-mode))
(add-hook 'vcl-mode-hook
          (lambda ()
            (setq vcl-indent-level 4)))

;;;;;; puppet mode
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;;;;;; nginx mode
(autoload 'nginx-mode "nginx-mode" nil t)

;;;;;; dockerfile mode
(use-package dockerfile-mode)

;;; init-conf.el ends here
