;;; init-helm.el --- initialize helm
;;; Commentary:
;;; Code:

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c y"   . helm-show-kill-ring)
         ("C-c m"   . helm-man-woman)
         ("C-c o"   . helm-occur)
         ;; enable usual C-h on helm
         :map helm-map
         ("C-h" . delete-backward-char)
         :map helm-find-files-map
         ("C-h" . delete-backward-char))
  :init
  (custom-set-faces
   '(helm-header           ((t (:background "#3a3a3a" :underline nil))))
   '(helm-source-header    ((t (:background "gray16" :foreground "gray64" :slant italic))))
   '(helm-candidate-number ((t (:foreground "#00afff"))))
   '(helm-selection        ((t (:background "#005f87" :weight normal))))
   '(helm-match            ((t (:foreground "darkolivegreen3")))))
  :config
  (helm-mode 1))

(use-package helm-ls-git)

;; dispaly helm by popwin
(push '("^\\*helm" :regexp t) popwin:special-display-config)
;; https://github.com/emacs-helm/helm/wiki/Popwin
(defun helm-popwin-help-mode-off ()
  "Turn `popwin-mode' off for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (popwin:display-buffer helm-buffer t)
    (customize-set-variable 'popwin:special-display-config
                            (delq 'help-mode popwin:special-display-config))))
(defun helm-popwin-help-mode-on ()
  "Turn `popwin-mode' on for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (customize-set-variable 'popwin:special-display-config
                            (add-to-list 'popwin:special-display-config 'help-mode nil #'eq))))
(add-hook 'helm-after-initialize-hook #'helm-popwin-help-mode-off)
(add-hook 'helm-cleanup-hook #'helm-popwin-help-mode-on)

;; enter helm-occur from isearch
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)

;; complete filename by TAB in helm-find-files and helm-read-file
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map  (kbd "TAB") 'helm-execute-persistent-action)
;; and prevent to create new buffer by TAB + TAB
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists."
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

;; for projectile
(use-package helm-projectile
  :diminish projectile-mode
  :bind ("C-c p p" . helm-projectile-switch-project)
  :init
  (use-package helm-ag)
  :config
  (projectile-global-mode t)
  (helm-projectile-on))

;;; init-helm.el ends here
