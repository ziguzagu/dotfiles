;; mcomplete
;;   * elisp: http://homepage1.nifty.com/bmonkey/emacs/elisp/mcomplete.el
(require 'mcomplete)
(turn-on-mcomplete-mode)

;; incremental-searching on minibuffer history
;;   elisp: http://www.sodan.org/~knagano/emacs/minibuf-isearch/
;; (require 'minibuf-isearch)
(setq completion-ignore-case t)

;; enable iswitchb-mode
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
