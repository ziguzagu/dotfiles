(use-package helm
  :pin melpa-stable
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c y"   . helm-show-kill-ring)
         ("C-c m"   . helm-man-woman)
         ("C-c o"   . helm-occur)
         ("C-c s"   . helm-imenu)
         :map isearch-mode-map
         ("C-o" . helm-occur-from-isearch)
         :map helm-map
         ("C-h" . delete-backward-char)
         :map helm-find-files-map
         ("C-h" . delete-backward-char)
         ("TAB" . helm-execute-persistent-action)
         :map helm-read-file-map
         ("TAB" . helm-execute-persistent-action))
  :config
  (helm-mode 1)
  :custom
  (helm-truncate-lines t)
  (helm-buffer-max-length 35)
  (helm-delete-minibuffer-contents-from-point t)
  (helm-ff-skip-boring-files t)
  (helm-boring-file-regexp-list '("~$" "\\.elc$" "^#" "/\\.$" "/\\.\\.$"))
  (helm-boring-buffer-regexp-list '("\\` " "\\*helm" "\\*vc" "\\*Annotate "))
  :custom-face
  (helm-header           ((t (:inherit 'header-line :inverse-video t))))
  (helm-source-header    ((t (:background "#292929" :foreground "#a3a3a3" :slant italic))))
  (helm-candidate-number ((t (:foreground "#5fafff" :background "#444444"))))
  (helm-selection        ((t (:background "#cb0000" :weight normal))))
  (helm-match            ((t (:foreground "#a2cd5a")))))

(use-package helm-ls-git
  :bind (("C-x f" . helm-browse-project)))

(use-package helm-git-grep
  :bind (("C-c g" . helm-git-grep)
         :map isearch-mode-map
         ("C-c g" . helm-git-grep-from-isearch)))

;; dispaly helm by popwin
(push '("^\\*helm" :regexp t) popwin:special-display-config)
;; https://github.com/emacs-helm/helm/wiki/Popwin
(defun my:helm-popwin-help-mode-off ()
  "Turn `popwin-mode' off for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (popwin:display-buffer helm-buffer t)
    (customize-set-variable 'popwin:special-display-config
                            (delq 'help-mode popwin:special-display-config))))
(defun my:helm-popwin-help-mode-on ()
  "Turn `popwin-mode' on for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (customize-set-variable 'popwin:special-display-config
                            (add-to-list 'popwin:special-display-config 'help-mode nil #'eq))))
(add-hook 'helm-after-initialize-hook #'my:helm-popwin-help-mode-off)
(add-hook 'helm-cleanup-hook #'my:helm-popwin-help-mode-on)

;; and prevent to create new buffer by TAB + TAB
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists."
  (when (file-exists-p candidate)
    ad-do-it))

(use-package helm-ag
  :config
  (setq helm-ag-base-command "rg --no-heading"))

(use-package helm-projectile
  :bind ("C-c p p" . helm-projectile-switch-project)
  :init
  (setq projectile-completion-system 'helm)
  :config
  (projectile-global-mode t)
  (helm-projectile-on))
