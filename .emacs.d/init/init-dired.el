(require 'wdired)
(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)

;; ls options
(setq insert-directory-program "gls") ;; homebrew
(setq dired-listing-switches "-AlhXF --color=auto --group-directories-first")
;; recursive copy/delete
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
;; don't create new buffer at moving direcotry
(put 'dired-find-alternate-file 'disabled nil)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; uniqufy
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; using ffap
(ffap-bindings)
(setq read-file-name-completion-ignore-case t)
