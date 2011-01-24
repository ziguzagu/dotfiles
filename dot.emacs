;;;; -*- mode: lisp-interaction; syntax: elisp; coding: iso-2022-7bit -*-

;;;;;; Basics
(cd "~")
(setq user-full-name "Hiroshi Sakai")
(setq user-mail-address "ziguzagu@gmail.com")
(setq load-path
      (append (list "~/.emacs.d"
                    "~/.emacs.d/lisp") load-path))
;; using unsafe local variables..?
(setq safe-local-variable-values (quote ((syntax . elisp))))

;;;;;; Languages
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)


;;;;;; EmacsClient
(add-hook 'after-init-hook 'server-start)
(shell-command "echo $WINDOW >~/.emacs.d/emacs-server-window")
(add-hook 'emacs-kill-hook
          (lambda ()
            (shell-command
             "rm ~/.emacs.d/emacs-server-window")))
(add-hook 'server-done-hook
          (lambda ()
            (shell-command
             "screen -X select `cat ~/.emacs.d/emacsclient-caller`")))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


(require 'init-appearance)


;;;;;; common editting
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;(setq fill-column (setq auto-fill-mode nil)
;; scroll by line
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
;; scroll by line with holding cursor.
(defun scroll-up-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))
(defun scroll-down-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))
(global-set-key "\M-p" 'scroll-up-in-place)
(global-set-key "\M-n" 'scroll-down-in-place)
;; comment
(setq comment-style 'multi-line)
;; auto-complete
;;  * http://www.emacswiki.org/emacs/AutoComplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/ac-dict")
(ac-config-default)
;; enable upcase/downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;;;;; anything
;;  * http://www.emacswiki.org/emacs/Anything
(require 'anything-config)
;; call show-kill-ring function by hand
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
;; minimal anything
(defun my-anything ()
  (interactive)
  (anything-other-buffer '(anything-c-source-buffers+
                           anything-c-source-recentf
                           anything-c-source-files-in-current-dir+)
                         "*my anything*"))
(global-set-key (kbd "C-c ;") 'my-anything)
;; color-moccur + anything
(require 'color-moccur)
(setq moccur-split-word t)
(require 'anything-c-moccur)
(setq anything-c-moccur-enable-auto-look-flag t)
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur)


;;;;;; autosave / backup
(setq auto-save-default t)
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/bakcup"))
            backup-directory-alist))
(setq backup-by-copying t)
(setq version-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq delete-old-versions t)


;;;;;; Global key bind
;; autoindent at entering newline
(global-set-key "\r" 'newline-and-indent)
;; goto
(global-set-key "\C-cg" 'goto-line)
;; help
(global-set-key "\C-ch" 'help-for-help)
;; backspace
(global-set-key "\C-h" 'delete-backward-char)


;;;;;; Misc
;; move divided windows by shift with cursor.
(windmove-default-keybindings)
;; woman
(global-set-key "\C-cm" 'woman)
;; re-load a file when it was changed by another process
(global-auto-revert-mode t)


;;;;;; window
;; vertical <-> horizontal
(defun window-toggle-split ()
  "toggle splitted windows vertical and horizontal"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "no splitted windows"))
  (let (before-height (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally))
    (switch-to-buffer-other-window other-buf)
    (other-window -1)))
(global-set-key "\C-x9" 'window-toggle-split)


;;;;;; minibuffer
;; powerful complete on minibuffer
;;   elisp: http://homepage1.nifty.com/bmonkey/emacs/elisp/mcomplete.el
(require 'mcomplete)
(turn-on-mcomplete-mode)
;; incremental-searching on minibuffer history
;;   elisp: http://www.sodan.org/~knagano/emacs/minibuf-isearch/
;; (require 'minibuf-isearch)
(setq completion-ignore-case t)


;;;;;; Dired
(add-hook 'dired-load-hook
          '(lambda ()
                  ;; show directories at the top of buffer
                  (setq ls-lisp-dirs-first t)
                  ;; ls options
                  (setq dired-listing-switches "-AlhFG")
                  ;; recursive copy/delete
                  (setq dired-recursive-copies 'always)
                  (setq dired-recursive-deletes 'always)))
;; don't create new buffer at moving direcotry
(put 'dired-find-alternate-file 'disabled nil)
;; a lump-sum rename
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
;; don't create new buffer when move to subdirectory
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;; uniqufy
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")
;; using ffap
(ffap-bindings)
(setq read-file-name-completion-ignore-case t)


;;;;;; shell
;; completation on shell-mode
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")
;; hide password
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)
;; handle escape sequence
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;;;;;; vc
(setq vc-follow-symlinks t)
(setq vc-make-backup-files t)


;;;;;; generic config file
(require 'generic-x)
(setq auto-mode-alist
      (append '(("\\.conf$" . default-generic-mode)
                ("\\.cfg$"  . default-generic-mode)) auto-mode-alist))

;;;;;; perl
(require 'init-perl)

;;;;;; c/c++
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4)
            (define-key c++-mode-map "\C-cc" 'compile)
            ))
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "stroustrup")
            ))
(setq auto-mode-alist
      (append '(("\\.c$" . c++-mode)
                ("\\.cpp$" . c++-mode)
                ("\\.h$" . c++-mode)) auto-mode-alist))


;;;;;; HTML editting
;; html-helper-mode
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist
      (append '(("\\.html$" . html-helper-mode)
                ("\\.tmpl$" . html-helper-mode)
                ("\\.tt$" . html-helper-mode)) auto-mode-alist))
;; HTML escape
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


;;;;;; nxml/nxhtml mode
;(load "~/.emacs.d/lisp/nxml/autostart.el")


;;;;;; css-mode
(autoload 'css-mode "css-mode")
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 4)
(setq auto-mode-alist
      (append '(("\\.css$" . css-mode)) auto-mode-alist))


;;;;;; For javascript editting
(autoload 'js2-mode "js2-mode" nil t)
(setq auto-mode-alist
      (append '(("\\.js$" . js2-mode)) auto-mode-alist))


;;;;;; dabbrev
;; elisp:
;;   * http://www.namazu.org/~tsuchiya/elisp/dabbrev-ja.el
;;   * http://www.namazu.org/~tsuchiya/elisp/dabbrev-highlight.el
(load "dabbrev-ja")
(require 'dabbrev-highlight)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace nil)
(global-set-key "\C-o" 'dabbrev-expand)


;;;;;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")


;;;;;; iswitchb-mode
(iswitchb-mode 1)
(add-hook 'iswitchb-define-mode-map-hook
          (lambda ()
            (define-key iswitchb-mode-map "\C-n" 'iswitchb-next-match)
            (define-key iswitchb-mode-map "\C-p" 'iswitchb-prev-match)
            (define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
            (define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)))
;; ignoreing buffers
(setq iswitchb-buffer-ignore
      '("^ "
        "*Messages*"
        "*Buffer*"
        "*Shell Command Output*"
        "Completions"))


;;;;;; migemo
(when (locate-library "migemo")
  (setq migemo-directory "/usr/share/migemo")
  (load "migemo")
  (migemo-init)
  ;; cache
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)
  ;; delay for accepting STDOUT
  (setq migemo-accept-process-output-timeout-msec 80)
  ;; turn off migemo wheren copy strings from buffer
  (defadvice isearch-yank-string
    (before migemo-off activate)
    (setq migemo-isearch-enable-p nil))
  ;; turn on migemo when searching by isearch
  (defadvice isearch-mode
    (before migemo-on activate)
    (setq migemo-isearch-enable-p t))
  )

;;;;;; *scratch*
;; save and restore the content of *scratch* buffer
(defun save-scratch-data ()
  (let ((str (progn
               (set-buffer (get-buffer "*scratch*"))
               (buffer-substring-no-properties
                (point-min) (point-max))))
        (file "~/.emacs.d/scratch"))
    (if (get-file-buffer (expand-file-name file))
        (setq buf (get-file-buffer (expand-file-name file)))
      (setq buf (find-file-noselect file)))
    (set-buffer buf)
    (erase-buffer)
    (insert str)
    (save-buffer)))

(defadvice save-buffers-kill-emacs
  (before save-scratch-buffer activate)
  (save-scratch-data))

(defun read-scratch-data ()
  (let ((file "~/.emacs.d/scratch"))
    (when (file-exists-p file)
      (set-buffer (get-buffer "*scratch*"))
      (erase-buffer)
      (insert-file-contents file))
    ))

(read-scratch-data)

;; make not to kill *scratch* buffer
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" $B$r:n@.$7$F(B buffer-list $B$KJ|$j9~$`(B
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(defun my-buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))

;; *scratch* $B%P%C%U%!$G(B kill-buffer $B$7$?$iFbMF$r>C5n$9$k$@$1$K$9$k(B
(add-hook 'kill-buffer-query-functions
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my-make-scratch 0) nil)
                        t))))

;; *scratch* $B%P%C%U%!$NFbMF$rJ]B8$7$?$i(B *scratch* $B%P%C%U%!$r?7$7$/:n$k(B
(add-hook 'after-save-hook
          (function (lambda ()
                      (unless (member "*scratch*" (my-buffer-name-list))
                        (my-make-scratch 1)))))


;;;;;; yaml-mode
(autoload 'yaml-mode "yaml-mode" nil t)
(setq auto-mode-alist
      (append '(("\\.yml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)) auto-mode-alist))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;;;;;; sdic
(when (locate-library "sdic")
  (autoload 'sdic-describe-word
    "sdic" "$B1QC18l$N0UL#$rD4$Y$k(B" t nil)
  (global-set-key "\C-cw" 'sdic-describe-word)
  (autoload 'sdic-describe-word-at-point
    "sdic" "$B%+!<%=%k$N0LCV$N1QC18l$N0UL#$rD4$Y$k(B" t nil)
  (global-set-key "\C-cW" 'sdic-describe-word-at-point)
  ;; $B1QOB8!:w$G;HMQ$9$k<-=q(B
  (setq sdic-eiwa-dictionary-list
        '((sdicf-client "~/opt/dict/eijirou.sdic")))
  ;; $BOB1Q8!:w$G;HMQ$9$k<-=q(B
  (setq sdic-waei-dictionary-list
        '((sdicf-client "~/opt/dict/waeijirou.sdic")))
  ;; $BJ8;z?'(B
  (setq sdic-face-color "brightmagenta")
  )
