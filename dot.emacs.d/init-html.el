(autoload 'sgml-mode "sgml-mode" nil t)
(setq auto-mode-alist
      (append '(("\\.html$" . sgml-mode)
                ("\\.tmpl$" . sgml-mode)
                ("\\.tt$" . sgml-mode)) auto-mode-alist))

(autoload 'css-mode "css-mode")
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 2)
(setq auto-mode-alist
      (append '(("\\.css$" . css-mode)) auto-mode-alist))

(autoload 'js2-mode "js2-mode" nil t)
(setq auto-mode-alist
      (append '(("\\.js$" . js2-mode)) auto-mode-alist))

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
