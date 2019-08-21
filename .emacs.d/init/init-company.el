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
  (set-face-attribute 'company-tooltip nil
                      :foreground "black"
                      :background "gray72")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black"
                      :background "gray72"
                      :underline t)
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white"
                      :background "steelblue"
                      :underline t)
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "white"
                      :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :inherit 'company-tooltip
                      :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "darkorange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40"))
