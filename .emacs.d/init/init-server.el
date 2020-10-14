(require 'server)
(unless (server-running-p)
  (server-start)
  (when (getenv "TMUX")
    (shell-command "tmux display -p '#I' > ~/.emacs.d/emacs-server-window")
    (add-hook 'kill-emacs-hook
              (lambda ()
                (shell-command "rm -f ~/.emacs.d/emacs-server-window"))))
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
