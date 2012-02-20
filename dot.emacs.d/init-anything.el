(require 'anything-config)

;; call show-kill-ring function by hand
(global-set-key (kbd "M-y") 'anything-show-kill-ring)

;; minimal anything
(setq recentf-max-saved-items 1000)
(defun my-anything ()
  (interactive)
  (anything-other-buffer '(anything-c-source-buffers+
                           anything-c-source-recentf
                           anything-c-source-files-in-current-dir+)
                         "*my anything*"))
(global-set-key (kbd "C-c ;") 'my-anything)

;; woman
(defun my-anything-woman ()
  (interactive)
  (anything-other-buffer '(anything-c-source-man-pages)
                         "*my woman*"))
(global-set-key (kbd "C-c m") 'my-anything-woman)

;; color-moccur + anything
(require 'color-moccur)
(setq moccur-split-word t)
(require 'anything-c-moccur)
(setq anything-c-moccur-enable-auto-look-flag t)
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur)
