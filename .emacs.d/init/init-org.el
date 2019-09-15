(use-package org
  :bind (("C-c c" . org-capture))

  :config
  (setq org-directory "~/Dropbox/org/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-startup-truncated nil)
  (setq org-startup-folded nil)
  (setq org-return-follows-link t)
  ;; org-capture
  (setq org-capture-templates
        '(("m" "Memo" entry (file+datetree (concat org-directory "memo.org"))
           "* [%<%H:%M>] %?\n"))
  ;; hilight lines in code block
  (setq org-src-fontify-natively t))
