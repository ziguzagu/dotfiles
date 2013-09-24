;; ls options
(setq dired-listing-switches "-alhFG --group-directories-first")

;; recursive copy/delete
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)))

;; don't create new buffer at moving direcotry
(put 'dired-find-alternate-file 'disabled nil)

;; a lump-sum rename
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; don't create new buffer when move to subdirectory
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; uniqufy
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; using ffap
(ffap-bindings)
(setq read-file-name-completion-ignore-case t)
