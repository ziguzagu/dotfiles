(use-package web-mode
  :mode ("\\.html\\'" "\\.tmpl\\'" "\\.tt\\'" "\\.tx\\'")
  :config
  (defun my:web-mode-hook ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2)
    (setq web-mode-engines-alist
          '(("template-toolkit" . "\\.tt\\'")
            ("template-toolkit" . "\\.tx\\'"))))
  (add-hook 'web-mode-hook 'my:web-mode-hook))

(use-package scss-mode
  :mode ("\\.css\\'" "\\.scss\\'")
  :config
  (setq css-indent-offset 2)
  (setq scss-compile-at-save nil))

(defun my:escape-html-region (start end)
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
