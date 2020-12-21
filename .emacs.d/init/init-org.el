(use-package org
  :bind (("C-c c" . org-capture))

  :config
  (setq org-directory "~/Dropbox/org/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-startup-truncated nil)
  (setq org-startup-folded nil)
  (setq org-return-follows-link t)
  (setq org-src-fontify-natively t) ;; hilight lines in code block
  (setq org-log-done 'time) ;; insert `CLOSED [timestamp]` after the headline

  ;; blog template
  (defun my:blog-subtree-post-capture-template ()
    "Returns `org-capture' template string for new blog post."
    (let ((section (format-time-string "posts/%Y/%m/" (org-current-time)))
          (date (format-time-string "%Y-%m-%d" (org-current-time))))
      (mapconcat 'identity
                 `("** TODO %?"
                   "  :PROPERTIES:"
                   "  :EXPORT_FILE_NAME: "
                   ,(concat "  :EXPORT_DATE: " date)
                   ,(concat "  :EXPORT_HUGO_SECTION: " section)
                   "  :END:"
                   "\n")
                 "\n")))

  ;; org-capture
  (setq org-capture-templates
        '(("m" "Memo" entry (file+datetree (lambda () (concat org-directory "memo.org")))
           "* [%<%H:%M>] %?\n")
          ("b" "Blog Posts" entry (file+olp "~/src/ziguzagu.org/blog.org" "Blog Posts")
           (function my:blog-subtree-post-capture-template)
           :prepend t :empty-lines 1))))

(use-package ox-hugo
  :pin melpa
  :after ox)
