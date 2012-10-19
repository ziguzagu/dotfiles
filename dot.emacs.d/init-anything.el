(el-get 'sync 'anything)
(require 'anything-config)
(require 'anything-match-plugin)

;; call show-kill-ring function by hand
(global-set-key (kbd "M-y") 'anything-show-kill-ring)

;; alternative to switch buffer / find recentf by antyhing
(setq recentf-max-saved-items 1000)
(defun my-switch-buffer ()
  (interactive)
  (anything-other-buffer '(anything-c-source-buffers+
                           anything-c-source-recentf
                           anything-c-source-files-in-current-dir+)
                         "*my-switch-buffer*"))
(global-set-key (kbd "C-c ;") 'my-swithc-buffer)

;; woman
(defun my-woman ()
  (interactive)
  (anything-other-buffer '(anything-c-source-man-pages)
                         "*woman*"))
(global-set-key (kbd "C-c m") 'my-woman)

;; color-moccur and ...
(el-get 'sync 'color-moccur)
(require 'color-moccur)
(setq moccur-split-word t)

;; then anything-c-moccur
(el-get 'sync 'anything-c-moccur)
(require 'anything-c-moccur)
(setq anything-c-moccur-enable-auto-look-flag t)
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur)

;; improve anything UI
(el-get 'sync 'anything-show-completion)
(require 'anything-show-completion)

;; popwin with anything
(setq anything-samewindow nil)
(push '("*my-switch-buffer*" :height 20) popwin:special-display-config)
