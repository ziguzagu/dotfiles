(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c y"   . helm-show-kill-ring)
         ("C-c m"   . helm-man-woman)
         ("C-c o"   . helm-occur)
         ("C-c s"   . helm-imenu)
         :map helm-map
         ("C-h" . delete-backward-char)
         :map helm-find-files-map
         ("C-h" . delete-backward-char)
         ("TAB" . helm-execute-persistent-action)
         :map helm-read-file-map
         ("TAB" . helm-execute-persistent-action))
  :config
  (set-face-attribute 'helm-header nil :inherit 'header-line :inverse-video t)
  (set-face-attribute 'helm-source-header nil :background "#292929" :foreground "#a3a3a3" :slant 'italic)
  (set-face-attribute 'helm-candidate-number nil :foreground "#5fafff" :background nil)
  (set-face-attribute 'helm-selection nil :background "#005f87" :weight 'normal)
  (set-face-attribute 'helm-match nil :foreground "#a2cd5a")
  (helm-mode 1))

(use-package helm-ls-git)

(use-package helm-git-grep
  :bind (("C-x g" . helm-git-grep)
         :map isearch-mode-map
         ("C-x g" . helm-git-grep-from-isearch)))

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

;; and prevent to create new buffer by TAB + TAB
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists."
  (when (file-exists-p candidate)
    ad-do-it))

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

(use-package helm-ag
  :config
  (setq helm-ag-base-command "rg --no-heading"))

(use-package helm-projectile
  :diminish projectile-mode
  :bind ("C-c p p" . helm-projectile-switch-project)
  :config
  (projectile-global-mode t)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))
