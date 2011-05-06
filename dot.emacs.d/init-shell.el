;; completation on shell-mode
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

;; hide password
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; handle escape sequence
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
