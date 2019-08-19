(use-package counsel
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-switch-buffer)
         ("C-x g" . counsel-git-grep)
         ("C-c s" . counsel-imenu)
         ("C-c y" . counsel-yank-pop)
         ("C-c o" . ivy-occur))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 20)
  :config
  (ivy-mode 1)
  (custom-set-faces
   '(ivy-current-match ((t (:inherit ivy-cursor))))
   '(ivy-minibuffer-match-face-1 ((t (:inherit region))))
   '(ivy-minibuffer-match-face-2 ((t (:inherit match))))
   '(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2))))
   '(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-2))))))

(use-package counsel-projectile
  :diminish projectile-mode
  :bind (("C-c p p" . counsel-projectile-switch-project)
         ("C-c p f" . counsel-projectile-find-file-dwim))
  :config
  (counsel-projectile-mode))
