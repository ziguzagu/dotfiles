;;;; -*- mode: lisp-interaction; syntax: elisp; coding: iso-2022-7bit -*-

;;;;;; Basics
(cd "~")
(setq user-full-name "Hiroshi Sakai")
(setq user-mail-address "ziguzagu@gmail.com")
(setq load-path
      (append (list "~/.emacs.d"
                    "~/.emacs.d/lisp"
                    "~/.emacs.d/lisp/blgrep")
              load-path))
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


(require 'init-color)


;;;;;; common editting
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;(setq fill-column (setq auto-fill-mode nil)
;; highlight selected region
(setq-default transient-mark-mode t)
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
(global-set-key "\C-c;" 'comment-or-uncomment-region)
(setq comment-style 'multi-line)
;; auto-complete
;;  * http://www.emacswiki.org/emacs/AutoComplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/ac-dict")
(ac-config-default)


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
;; move with HOME/END
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key "\C-cg" 'goto-line)
(global-set-key [f1] 'help-for-help)
;; backspace
(global-set-key "\C-h" 'delete-backward-char)
;; modifiers for Cocoa Emacs
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))


;;;;;; Misc
(setq inhibit-startup-message t)
;; move divided windows by shift with cursor.
(windmove-default-keybindings)
;; hide menu bar
(menu-bar-mode -1)
;; resize the temp buffer when necessary
(temp-buffer-resize-mode 1)
;; woman
(global-set-key "\C-cm" 'woman)
;; turn off both beep and visual bell.
(setq ring-bell-function 'ignore)
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


;;;;;; recent file mode
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 15)
(define-key global-map "\C-cr" 'recentf-open-files)


;;;;;; modeline
;; show line/column numer
(line-number-mode t)
(column-number-mode t)


;;;;;; minibuffer
;; powerful complete on minibuffer
;;   elisp: http://homepage1.nifty.com/bmonkey/emacs/elisp/mcomplete.el
(require 'mcomplete)
(turn-on-mcomplete-mode)
;; fix mini-buffer
(setq resize-mini-windows nil)
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
                  (setq dired-recursive-deletes 'always)
                  ;; dummy command
                  (put 'dired-find-alternate-file 'disabled nil)))
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


;;;;;; autoinsert
(require 'autoinsert)
(setq auto-insert-directory "~/.emacs.d/template/")
(setq auto-insert-alist
      (nconc '(("\\.cgi$" . "perl.pl")
               ("\\.pl$" . "perl.pl")
               ("\\.pm$" . "perl.pm")
               ("\\.t$" . "perl.t")
               ("\\.html$" . "html.html")
               ("\\.tt$" . "html.html"))
             auto-insert-alist))
(add-hook 'find-file-not-found-hooks 'auto-insert)
(setq auto-insert-query nil)


;;;;;; snippet and abbrev
;; elisp: http://www.kazmier.com/computer/snippet.el
(require 'snippet)
(setq-default abbrev-mode t)
(snippet-with-abbrev-table 'global-abbrev-table
                           ("ttv" . "[% $${val} %]$.")
                           ("ttfor" . "[% FOR $${local} IN $${val} -%]\n$>$.\n$>[%- END %]\n")
                           ("ttif" . "[% IF $${condition} -%]\n$>$.\n[%- END %]\n")
                           ("ttunless" . "[% UNLESS $${condition} -%]\n$>$.\n[%- END %]\n"))


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
;; urlencode
;;(load "urlencode")
;; snippets
(defvar html-helper-mode-abbrev-table nil)
(define-abbrev-table 'html-helper-mode-abbrev-table ())
(snippet-with-abbrev-table 'html-helper-mode-abbrev-table
                           ;; basics
                           ("hthref" . "<a href=\"$${url}\" title=\"$${title}\">$${text}</a>")
                           ("htinput" . "<input type=\"$${type}\" name=\"$${name}\" value=\"$${value}\" />")
                           ("htimg" . "<img src=\"$${url}\" alt=\"$${alt}\" />")
                           ("htform" . "<form action=\"$${url}\" method=\"$${get or post}\">\n$>$.\n$></form>\n")
                           ;; for HTML::Template
                           ("htincl" . "<TMPL_INCLUDE NAME=\"$${tmpl}\">")
                           ("htvar" . "<TMPL_VAR NAME=$${var}>")
                           ;; for TP/MT templates
                           ("mttrans" . "<MT_TRANS phrase=\"$${var}\">")
                           ("mtset" . "<$MTSetVar name=\"$${name}\" value=\"$${value}\"$>")
                           ("mtget" . "<$MTGetVar name=\"$${name}\"$>")
                           ("mtif"  . "<MTIfVar name=\"$${name}\">$${text}</MTIfVar>"))

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


;;;;;; actionscript
(autoload 'actionscript-mode "actionscript-mode" nil t)
(setq auto-mode-alist
      (append '(("\\.as$" . actionscript-mode)) auto-mode-alist))


;;;;;; kill-summary
;; elisp: http://mibai.tec.u-ryukyu.ac.jp/~oshiro/Programs/elisp/kill-summary.el
(autoload 'kill-summary "kill-summary" nil t)
(define-key global-map "\ey" 'kill-summary)


;;;;;; dabbrev
;; elisp:
;;   * http://www.namazu.org/~tsuchiya/elisp/dabbrev-ja.el
;;   * http://www.namazu.org/~tsuchiya/elisp/dabbrev-highlight.el
(load "dabbrev-ja")
(require 'dabbrev-highlight)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace nil)
(global-set-key "\C-o" 'dabbrev-expand)


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
    ;; "*scratch*" を作成して buffer-list に放り込む
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

;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
(add-hook 'kill-buffer-query-functions
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my-make-scratch 0) nil)
                        t))))

;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
(add-hook 'after-save-hook
          (function (lambda ()
                      (unless (member "*scratch*" (my-buffer-name-list))
                        (my-make-scratch 1)))))


;;;;;; changelog memo
(autoload 'clmemo "clmemo" "ChangeLog memo mode." t)
(setq clmemo-file-name "~/.emacs.d/clmemo.txt")
(global-set-key "\C-xM" 'clmemo)
;; insert region to clmemo
(setq clmemo-buffer-function-list
      '(clmemo-insert-region))
;; add weekday info
(setq clmemo-time-string-with-weekday t)
;; clgrep
(autoload 'clgrep "clgrep" "ChangeLog grep." t)
(autoload 'clgrep-item "clgrep" "ChangeLog grep." t)
(autoload 'clgrep-item-header "clgrep" "ChangeLog grep for item header" t)
(autoload 'clgrep-item-tag "clgrep" "ChangeLog grep for tag" t)
(autoload 'clgrep-item-notag "clgrep" "ChangeLog grep for item except for tag" t)
(autoload 'clgrep-item-nourl "clgrep" "ChangeLog grep item except for url" t)
(autoload 'clgrep-entry "clgrep" "ChangeLog grep for entry" t)
(autoload 'clgrep-entry-header "clgrep" "ChangeLog grep for entry header" t)
(autoload 'clgrep-entry-no-entry-header "clgrep" "ChangeLog grep for entry except entry header" t)
(autoload 'clgrep-entry-tag "clgrep" "ChangeLog grep for tag" t)
(autoload 'clgrep-entry-notag "clgrep" "ChangeLog grep for tag" t)
(autoload 'clgrep-entry-nourl "clgrep" "ChangeLog grep entry except for url" t)
(add-hook 'clmemo-mode-hook
          '(lambda () (define-key clmemo-mode-map "\C-c\C-g" 'clgrep)))


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
    "sdic" "英単語の意味を調べる" t nil)
  (global-set-key "\C-cw" 'sdic-describe-word)
  (autoload 'sdic-describe-word-at-point
    "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
  (global-set-key "\C-cW" 'sdic-describe-word-at-point)
  ;; 英和検索で使用する辞書
  (setq sdic-eiwa-dictionary-list
        '((sdicf-client "~/opt/dict/eijirou.sdic")))
  ;; 和英検索で使用する辞書
  (setq sdic-waei-dictionary-list
        '((sdicf-client "~/opt/dict/waeijirou.sdic")))
  ;; 文字色
  (setq sdic-face-color "brightmagenta")
  )


(put 'downcase-region 'disabled nil)
