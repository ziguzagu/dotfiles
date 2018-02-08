;;; init-web.el --- customize web mode and frontend stuff
;;; Commentary:
;;; Code:

(use-package web-mode
  :mode ("\\.html" "\\.tmpl$" "\\.tt$" "\\.tx$")
  :config
  (defun my-web-mode-hook ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2)
    (setq web-mode-engines-alist
          '(("template-toolkit" . "\\.tt$")
            ("template-toolkit" . "\\.tx$"))))
  (add-hook 'web-mode-hook 'my-web-mode-hook))

;;;;;; css-mode
(use-package scss-mode
  :mode ("\\.css$" "\\.scss$")
  :config
  (setq css-indent-offset 2)
  (setq scss-compile-at-save nil))

;;;;;; js2-mode
(use-package js2-mode
  :mode (("\\.js$"   . js2-mode)
         ("\\.json$" . js2-mode)
         ("\\.jsx$"  . js2-jsx-mode))
  :interpreter ("node" . js2-mode)
  :config
  (defun my-js2-mode ()
    (custom-set-variables
     '(js2-basic-offset 2))
    (flycheck-mode))

  (add-hook 'js2-mode-hook 'my-js2-mode))

(use-package typescript-mode
  :mode "\\.ts\\'")

;;;;;; coffee-mode
(use-package coffee-mode
  :mode "\\.coffee$"
  :config
  (custom-set-variables
   '(coffee-tab-width 2)
   '(coffee-indent-like-python-mode t)))

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

;;; init-web.el ends here
