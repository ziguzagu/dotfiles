;;; init-helm.el --- initialize helm
;;; Commentary:
;;; Code:

(require 'helm-config)
(require 'helm-ls-git)
(require 'helm-man)

;; dispaly helm by popwin
(setq helm-samewindow nil)
(push '("^\*helm .+\*$" :regexp t :height 20) popwin:special-display-config)

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

;; increase to make helm-source-recentf useful
(setq recentf-max-saved-items 5000)

;; customize default
(custom-set-variables
 '(helm-truncate-lines t)
 '(helm-buffer-max-length 35)
 '(helm-delete-minibuffer-contents-from-point t)
 ;; skip boring files
 '(helm-ff-skip-boring-files t)
 '(helm-boring-file-regexp-list '("~$" "\\.elc$" "^#" "/\.$" "/\.\.$"))
 ;; shorten
 '(helm-ls-git-show-abs-or-relative 'relative)
 ;; additional source for helm-mini
 '(helm-mini-default-sources '(helm-source-buffers-list
                               helm-source-recentf
                               helm-source-ls-git
                               helm-source-buffer-not-found)))

;; enable usual C-h on helm
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

(provide 'init-helm)
;;; init-helm.el ends here
