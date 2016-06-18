;;; init-helm.el --- initialize helm
;;; Commentary:
;;; Code:

(use-package helm-config
  :config
  (use-package helm-ls-git)
  (use-package helm-man)
  (helm-mode 1))

;; dispaly helm by popwin
(setq helm-samewindow nil)
(push '("^\*helm" :regexp t :height 20) popwin:special-display-config)

;; key bindings
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c y")   'helm-show-kill-ring)
(global-set-key (kbd "C-c m")   'helm-man-woman)
(global-set-key (kbd "C-c o")   'helm-occur)

;; enter helm-occur from isearch
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)

;; complete filename by TAB in helm-find-files and helm-read-file
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map  (kbd "TAB") 'helm-execute-persistent-action)
;; and prevent to create new buffer by TAB + TAB
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists"
  (when (file-exists-p candidate)
    ad-do-it))

;; increase to make helm-source-recentf useful
(setq recentf-max-saved-items 5000)

;; customize default
(custom-set-variables
 '(helm-truncate-lines t)
 '(helm-buffer-max-length 35)
 '(helm-delete-minibuffer-contents-from-point t)
 ;; skip boring files
 '(helm-ff-skip-boring-files t)
 '(helm-boring-file-regexp-list '("~$" "\\.elc$" "^#" "/\\.$" "/\\.\\.$"))
 ;; shorten
 '(helm-ls-git-show-abs-or-relative 'relative)
 ;; additional source for helm-mini
 '(helm-mini-default-sources '(helm-source-buffers-list
                               helm-source-recentf
                               helm-source-ls-git
                               helm-source-buffer-not-found)))

;; enable usual C-h on helm
(define-key helm-map            (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

;; customize faces
(set-face-attribute 'helm-selection nil
                    :weight 'normal
                    :background "color-208"
                    :foreground "color-16")

;; for projectile
(use-package helm-projectile
  :diminish projectile-mode
  :bind ("C-c p p" . helm-projectile-switch-project)
  :init
  (use-package helm-ag)
  :config
  (projectile-global-mode t)
  (helm-projectile-on))

(provide 'init-helm)
;;; init-helm.el ends here
