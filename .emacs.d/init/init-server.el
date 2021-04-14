(when (and (daemonp) (getenv "TMUX"))
  (shell-command "tmux display -p '#I' > ~/.emacs.d/emacs-server-window")
  (add-hook 'kill-emacs-hook
            (lambda ()
              (shell-command "rm -f ~/.emacs.d/emacs-server-window"))))
