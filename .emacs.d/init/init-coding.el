(use-package diff-mode
  :custom-face
  (diff-header         ((t (:foreground "#a8a8a8" :background "#303030" :slant italic))))
  (diff-file-header    ((t (:inherit diff-header))))
  (diff-hunk-header    ((t (:inherit diff-header :background "#080808"))))
  (diff-index          ((t (:inherit diff-hunk-header :foreground "#5fafd7"))))
  (diff-function       ((t (:inherit diff-hunk-header :foreground "#af87d7"))))
  (diff-context        ((t (:inherit default))))
  (diff-added          ((t (:inherit default :foreground "#87af5f"))))
  (diff-removed        ((t (:inherit default :foreground "#d75f5f"))))
  (diff-refine-added   ((t (:inherit diff-added :background "#005f00"))))
  (diff-refine-removed ((t (:inherit diff-removed :background "#5f0000")))))

(use-package dash-at-point
  :bind (("C-c ." . dash-at-point)
         ("C-c C-." . dash-at-point-with-docset)))

(use-package sql
  :config
  (sql-highlight-mysql-keywords))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :config
  (set-face-attribute 'markdown-header-delimiter-face nil
                      :foreground "#ffa500")
  (set-face-attribute 'markdown-header-rule-face nil
                      :foreground "#ffa500")
  (set-face-attribute 'markdown-header-face nil
                      :foreground "#ffa500")
  (set-face-attribute 'markdown-inline-code-face nil
                      :foreground "#87d75f")
  (set-face-attribute 'markdown-pre-face nil
                      :foreground "#87d75f")
  (set-face-attribute 'markdown-language-keyword-face nil
                      :foreground "#858585")
  (set-face-attribute 'markdown-list-face nil
                      :foreground "#af87ff" :weight 'bold)
  (set-face-attribute 'markdown-link-face nil
                      :foreground "#5fafff"))

(use-package sh-script
  :init
  (setq sh-indentation 2)
  (setq sh-basic-offset 2)
  (setq sh-shell-file "/bin/bash"))

(use-package dumb-jump
  :bind (("M-." . dumb-jump-go)
         ("M-," . dumb-jump-back))
  :init
  (setq dumb-jump-selector 'helm)
  (setq dumb-jump-default-project nil))
