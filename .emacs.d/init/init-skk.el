(use-package ddskk
  :bind ("C-x C-j" . skk-mode)
  :init
  (setq skk-init-file "~/.config/ddskk/init")
  :config
  (setq skk-server-host "localhost"
        skk-server-portnum 1178
        skk-server-report-response t))
