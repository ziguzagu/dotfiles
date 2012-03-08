;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; popwin
(auto-install-from-url "https://github.com/m2ym/popwin-el/raw/master/popwin.el")
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
;; popwin with anything
(setq anything-samewindow nil)
(push '("*anything*" :height 20) popwin:special-display-config)

;; auto-complete
;;  * http://www.emacswiki.org/emacs/AutoComplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-dict")
(ac-config-default)

;; dabbrev
(auto-install-from-url "http://www.namazu.org/~tsuchiya/elisp/dabbrev-highlight.el")
(require 'dabbrev-highlight)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace nil)
(global-set-key "\C-o" 'dabbrev-expand)

;; yasnippet
(add-to-list 'load-path
             "~/.emacs.d/elisp/yasnippet")
(require 'yasnippet)
(yas/load-directory "~/.emacs.d/snippets")
(yas/global-mode 1)
