(use-package ddskk
  :bind ("C-x C-j" . skk-mode)
  :init
  (setq skk-init-file "~/.config/ddskk/init")
  (setq skk-user-directory "~/.local/share/ddskk")
  (setq default-input-method "japanese-skk")
  (setq skk-preload t)
  (add-hook 'skk-load-hook
            (lambda ()
              (require 'context-skk))))
