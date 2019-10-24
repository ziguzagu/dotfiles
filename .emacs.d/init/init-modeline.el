(make-face 'mode-line-vc-mode)
(make-face 'fc-error-face)
(make-face 'fc-warning-face)
(make-face 'fc-info-face)
(custom-set-faces
 '(mode-line-vc-mode   ((t (:foreground "#5fafff" :weight normal))))
 '(fc-error-face       ((t (:foreground "#fb4933" :weight normal))))
 '(fc-warning-face     ((t (:foreground "#fabd2f" :weight normal))))
 '(fc-info-face        ((t (:foreground "#83a598" :weight normal)))))

;; get rid of leading ' git:' from vc-mode
(defun my:vc-branch ()
  (let ((backend (vc-backend buffer-file-name)))
    (substring vc-mode 5)))

;; customize flycheck modeline display
(defun my:mode-line-checker-text (state)
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "?"))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running) (format "•%s" err))))
(defun my:mode-line-chcker ()
  (when (and (bound-and-true-p flycheck-mode)
             (or flycheck-current-errors
                 (eq 'running flycheck-last-status-change)))
    (cl-loop for state in '(error warning info)
             as ret = (my:mode-line-checker-text state)
             when ret
             concat (propertize
                     ret
                     'face (intern (format "fc-%S-face" state))))))

(setq-default mode-line-format
              (list " "
                    'mode-line-mule-info
                    'mode-line-modified
                    "  "
                    'mode-line-buffer-identification
                    '(:eval (format " [%s]" (projectile-project-name)))
                    '(vc-mode
                     ((:propertize "  " face mode-line-vc-mode)
                      (:propertize (:eval (my:vc-branch)) face mode-line-vc-mode)))
                    "  "
                    'mode-name
                    "  "
                    '(:eval (my:mode-line-chcker))
                    "  %c:%l(%p)"))
