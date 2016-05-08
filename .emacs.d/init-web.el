;;; init-web.el --- customize web mode and frontend stuff
;;; Commentary:
;;; Code:

(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tt$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.tx$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-engines-alist
      '(("template-toolkit" . "\\.tt$")
        ("template-toolkit" . "\\.tx$"))))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; JSX syntax highlighting
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))
;; flycheck by eslint in web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; disable jshint and json-jsonlist
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)
                      '(json-jsonlist)))

;;;;;; css-mode
(autoload 'scss-mode "scss-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.css$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-hook 'scss-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
            (setq scss-compile-at-save nil)))

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

;;;;;; coffee-mode
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(custom-set-variables
 '(coffee-tab-width 2)
 '(coffee-indent-like-python-mode t))

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
