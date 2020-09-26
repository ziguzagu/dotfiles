(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("e" . wdired-change-to-wdired-mode)
              ("RET" . dired-find-alternate-file))
  :init
  ;; use GNU ls installed by homebrew to use its own options, not have BSD ls.
  (setq insert-directory-program "gls"
        dired-listing-switches "-AlhXF --color=auto --group-directories-first")
  ;; recursive copy/delete
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)
  ;; don't create new buffer at moving direcotry
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  ;; use dired-jump
  (use-package dired-x
    :ensure nil
    :bind ("C-x C-d" . dired-jump)))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;; using ffap
(ffap-bindings)
(setq read-file-name-completion-ignore-case t)
