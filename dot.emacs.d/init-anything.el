(el-get 'sync 'anything)
(require 'anything-config)
(require 'anything-match-plugin)

;; call show-kill-ring function by hand
(global-set-key (kbd "M-y") 'anything-show-kill-ring)

;; minimal anything
(setq recentf-max-saved-items 1000)
(defun my-anything ()
  (interactive)
  (anything-other-buffer '(anything-c-source-buffers+
                           anything-c-source-recentf
                           anything-c-source-files-in-current-dir+)
                         "*minimal-anything*"))
(global-set-key (kbd "C-c ;") 'my-anything)

;; woman
(defun my-anything-woman ()
  (interactive)
  (anything-other-buffer '(anything-c-source-man-pages)
                         "*woman*"))
(global-set-key (kbd "C-c m") 'my-anything-woman)

;; color-moccur and ...
(require 'color-moccur)
(setq moccur-split-word t)

;; then anything-c-moccur
(require 'anything-c-moccur)
(setq anything-c-moccur-enable-auto-look-flag t)
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur)

;; improve anything UI
(require 'anything-show-completion)

;; popwin with anything
(setq anything-samewindow nil)
(push '("*minimal-anything*" :height 20) popwin:special-display-config)
