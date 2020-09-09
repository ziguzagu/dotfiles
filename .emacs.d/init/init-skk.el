(use-package ddskk
  :bind ("C-x C-j" . skk-mode)
  :init
  (setq skk-init-file "~/.config/ddskk/init")
  (add-hook 'skk-load-hook
            (lambda ()
              (require 'context-skk)))
  :config
  (setq skk-server-host "localhost"
        skk-server-portnum 1178
        skk-server-report-response t))
