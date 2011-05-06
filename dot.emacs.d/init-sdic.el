;;;; -*- mode: lisp-interaction; syntax: elisp; coding: iso-2022-7bit -*-

(when (locate-library "sdic")
  (autoload 'sdic-describe-word
    "sdic" "$B1QC18l$N0UL#$rD4$Y$k(B" t nil)
  (global-set-key "\C-cw" 'sdic-describe-word)
  (autoload 'sdic-describe-word-at-point
    "sdic" "$B%+!<%=%k$N0LCV$N1QC18l$N0UL#$rD4$Y$k(B" t nil)
  (global-set-key "\C-cW" 'sdic-describe-word-at-point)
  ;; $B1QOB8!:w$G;HMQ$9$k<-=q(B
  (setq sdic-eiwa-dictionary-list
        '((sdicf-client "~/dict/eijirou.sdic")))
  ;; $BOB1Q8!:w$G;HMQ$9$k<-=q(B
  (setq sdic-waei-dictionary-list
        '((sdicf-client "~/dict/waeijirou.sdic")))
  ;; $BJ8;z?'(B
  (setq sdic-face-color "brightmagenta"))
