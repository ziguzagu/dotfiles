;;; init-coding.el --- general coding settings
;;; Commentary:
;;; Code:

;; make compact vc-annotate display
(defadvice vc-git-annotate-command (around vc-git-annotate-command activate)
  "Suppress relative path of file from git blame output."
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))

;; open Pull Reuqest URL on this line from vc-annotate enter P as same as tig
(defun vc-annotate-open-pr-at-line ()
  "Open Pull Request URL at the line from git blame output."
  (interactive)
  (let* ((rev-at-line (vc-annotate-extract-revision-at-line))
         (rev (car rev-at-line)))
    (shell-command (concat "git hub open " rev))))
(define-key vc-annotate-mode-map (kbd "P") 'vc-annotate-open-pr-at-line)

;; flycheck
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-disabled-checker '(emacs-lisp-checkdoc))
  :config
  (custom-set-variables '(flycheck-mode-line-prefix "✓"))
  (custom-set-faces
   '(flycheck-warning ((t (:foreground "yellow" :underline t :weight normal))))
   '(flycheck-error   ((t (:foreground "red1" :underline t :weight normal))))))

(use-package flycheck-popup-tip
  :config
  (flycheck-popup-tip-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-mysql-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (set-face-attribute 'markdown-header-delimiter-face nil
                      :foreground "orange")
  (set-face-attribute 'markdown-header-rule-face nil
                      :foreground "orange")
  (set-face-attribute 'markdown-header-face nil
                      :foreground "orange")
  (set-face-attribute 'markdown-inline-code-face nil
                      :foreground "darkolivegreen3")
  (set-face-attribute 'markdown-pre-face nil
                      :foreground "darkolivegreen3")
  (set-face-attribute 'markdown-language-keyword-face nil
                      :foreground "gray52")
  (set-face-attribute 'markdown-list-face nil
                      :foreground "mediumpurple1" :weight 'bold)
  (set-face-attribute 'markdown-link-face nil
                      :foreground "color-75"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(add-hook 'before-save-hook #'gofmt-before-save)

;;; init-coding.el ends here
