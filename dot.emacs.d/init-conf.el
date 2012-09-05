;;;;;; generic config files
(setq auto-mode-alist
      (append '(("\\.conf$" . conf-mode)
                ("\\.cfg$"  . conf-mode)) auto-mode-alist))

;;;;;; yaml
(el-get 'sync 'yaml-mode)
(autoload 'yaml-mode "yaml-mode" nil t)
(setq auto-mode-alist
      (append '(("\\.yml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)) auto-mode-alist))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;;;; vcl mode
(el-get 'sync 'vcl-mode)
(autoload 'vcl-mode "vcl-mode" "Major mode for editting varnish config file")
(add-to-list 'auto-mode-alist '("\\.vcl$" . vcl-mode))
(add-hook 'vcl-mode-hook
          (lambda ()
            (setq vcl-indent-level 4)))

;;;;;; puppet mode
(el-get 'sync 'puppet-mode)
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
