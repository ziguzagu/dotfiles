(require 'vc-git)
(require 'vc-dir)
(require 'vc-annotate)

(setq vc-follow-symlinks t)
(setq vc-make-backup-files t)

;; make compact vc-annotate display
(defadvice vc-git-annotate-command (around vc-git-annotate-command activate)
  "Suppress relative path of file from git blame output."
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))

;; open Pull Reuqest URL on this line from vc-annotate enter P as same as tig
(defun my:open-pr-at-line ()
  "Open Pull Request URL at the line from git blame output."
  (interactive)
  (let* ((rev-at-line (vc-annotate-extract-revision-at-line))
         (rev (car rev-at-line)))
    (shell-command (concat "git hub open " rev))))
(define-key vc-annotate-mode-map (kbd "P") 'my:open-pr-at-line)

;; open current file by tig with blame mode
(defun my:tig-current-file ()
  (interactive)
  (shell-command
   (format "tmux new-window 'cd %s && tig blame +%s %s'"
           (file-name-directory buffer-file-name)
           (line-number-at-pos)
           (file-name-nondirectory buffer-file-name))))
(define-key vc-prefix-map [(t)] 'my:tig-current-file)

;; https://snarfed.org/emacs-vc-git-tweaks
;;
;; In vc-git and vc-dir for git buffers, make (C-x v) a run git add, u run git
;; reset, and r run git reset and checkout from head.
(defun my:vc-git-command (verb fn)
  (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset nil t)))
         (backend (car fileset-arg))
         (files (nth 1 fileset-arg)))
    (if (eq backend 'Git)
        (progn (funcall fn files)
               (message (concat verb " " (number-to-string (length files))
                                " file(s).")))
      (message "Not in a vc git buffer."))))

(defun my:vc-git-add (&optional revision vc-fileset comment)
  (interactive "P")
  (my:vc-git-command "Staged" 'vc-git-register))
(define-key vc-prefix-map [(a)] 'my:vc-git-add)
(define-key vc-dir-mode-map [(a)] 'my:vc-git-add)

(defun my:vc-git-reset (&optional revision vc-fileset comment)
  (interactive "P")
  (my:vc-git-command "Unstaged"
                     (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))
(define-key vc-prefix-map [(u)] 'my:vc-git-reset)
(define-key vc-dir-mode-map [(u)] 'my:vc-git-reset)
;; Remap vc-revert to `r` from `u`
(define-key vc-prefix-map [(r)] 'vc-revert)
(define-key vc-dir-mode-map [(r)] 'vc-revert)

;; hide up to date files after refreshing in vc-dir
(define-key vc-dir-mode-map [(g)]
  (lambda () (interactive) (vc-dir-refresh) (vc-dir-hide-up-to-date)))

;; enable aut-fill-mode at git commit editting for 50/72 rules
(add-hook 'vc-git-log-edit-mode-hook (lambda ()
                                       (setq fill-column 72)
                                       (turn-on-auto-fill)))

(use-package git-commit)

(use-package browse-at-remote
  :config
  (define-key vc-prefix-map [(w)] 'browse-at-remote))
