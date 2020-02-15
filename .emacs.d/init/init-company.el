(use-package company
  :bind (("C-o" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-filter-candidates)
         ("TAB" . company-complete-selection)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (global-company-mode t)
  ;; tweak backends
  (add-to-list 'company-backends 'company-dabbrev)
  :custom
  (company-idle-delay 0)
  (company-auto-expand t)
  (company-minimum-prefix-length 3)
  (company-dabbrev-minimum-length 3)
  ;; go to top at the next of last candidates
  (company-selection-wrap-around t)
  :custom-face
  (company-tooltip ((t (:foreground "gray3" :background "gray72"))))
  (company-tooltip-common ((t (:inherit 'company-tooltip :underline t))))
  (company-tooltip-selection ((t (:foreground "gray89" :background "steelblue"))))
  (company-tooltip-common-selection ((t (:inherit 'company-tooltip-selection :underline t))))
  (company-preview-common ((t (:inherit 'company-tooltip-common))))
  (company-scrollbar-fg ((t (:background "darkorange"))))
  (company-scrollbar-bg ((t (:background "gray40")))))

(use-package company-statistics
  :config
  (company-statistics-mode))
