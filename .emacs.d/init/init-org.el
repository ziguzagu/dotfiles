(use-package org
  :bind (("C-c c" . org-capture))
  :custom
  (org-directory "~/Dropbox/org/")
  (org-default-notes-file (concat org-directory "notes.org"))
  (org-startup-truncated nil)
  (org-startup-folded nil)
  (org-return-follows-link t)
  (org-src-fontify-natively t) ;; hilight lines in code block
  (org-log-done 'time) ;; insert `CLOSED [timestamp]` after the headline
  :init
  ;; blog template
  (defun my:blog-subtree-post-capture-template ()
    "Returns `org-capture' template for new blog post."
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
        '(("n" "Notes" entry (file+datetree (lambda () (concat org-directory "notes.org")))
           "* [%<%H:%M>] %?\n")
          ("b" "Blog Posts" entry (file+olp "~/src/ziguzagu.org/blog.org" "Blog Posts")
           (function my:blog-subtree-post-capture-template)
           :prepend t :empty-lines 1)))
  :config
  (require 'org-tempo))

(use-package ox-hugo
  :after ox)
