;;;;;; generic config files
(setq auto-mode-alist
      (append '(("\\.conf$" . conf-mode)
                ("\\.cfg$"  . conf-mode)) auto-mode-alist))

;;;;;; yaml
(autoload 'yaml-mode "yaml-mode" nil t)
(setq auto-mode-alist
      (append '(("\\.yml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)) auto-mode-alist))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

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
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
