;; yaml
(autoload 'yaml-mode "yaml-mode" nil t)
(setq auto-mode-alist
      (append '(("\\.yml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)) auto-mode-alist))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; generic config files
(require 'generic-x)
(setq auto-mode-alist
      (append '(("\\.conf$" . default-generic-mode)
                ("\\.cfg$"  . default-generic-mode)) auto-mode-alist))

;; vcl mode
(autoload 'vcl-mode "vcl-mode" "Major mode for editting varnish config file")
(add-to-list 'auto-mode-alist '("\\.vcl$" . vcl-mode))
(add-hook 'vcl-mode-hook
          (lambda ()
            (setq vcl-indent-level 4)))

;; puppet mode
(auto-install-from-url "https://raw.github.com/puppetlabs/puppet-syntax-emacs/master/puppet-mode.el")
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
