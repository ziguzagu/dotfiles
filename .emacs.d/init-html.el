;;;;;; use sgml-mode for editting HTML
(autoload 'sgml-mode "sgml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html$" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl$" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.tt$"   . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.tx$"   . sgml-mode))

;;;;;; css-mode
(el-get 'sync 'scss-mode)
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.css$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-hook 'sccs-mode
          (setq css-indent-offset 2)
          (setq scss-compile-at-save nil))

;;;;;; js2-mode
(el-get 'sync 'js2-mode)
(autoload 'js2-mode "js2-mode" nil t)
(setq-default js2-basic-offset 2)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;;;;;; utilities
(defun escape-html-region (start end)
  "Escape '&<>' characters in the region using '&amp;', '&lt;', and '&gt;'."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (search-forward "&" nil t)
        (replace-match "&amp;" nil t))
      (goto-char start)
      (while (search-forward "<" nil t)
        (replace-match "&lt;" nil t))
      (goto-char start)
      (while (search-forward ">" nil t)
        (replace-match "&gt;" nil t)))))
