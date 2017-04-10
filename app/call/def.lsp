;;; call/def.lsp --- Functions and Variables

;; Copyright (C) CALL Team and Contributors

;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; # Functions and Variables
;; =========================

;; ### defun
;; (**defun** ...) -> [ll-functionp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; defun-q
;; TODO

;; ### lambda
;; (**lambda** ...) -> [ll-functionp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### apply
;; (**apply** _fn_ _lst_) -> [ll-anyp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### function
;; (**function** ...)
;;;
;; TODO
;;;
;; AutoLISP built-in.

;; ### ll-functionp
;; (**ll-functionp** _v_) -> [ll-booleanp]
;; - _v_ : [ll-anyp]
;;
;; TODO
(defun ll-functionp (v)
  (or (member (type v) '(SUBR USUBR EXSUBR EXRXSUBR))))

;; ### boundp
;; (**boundp** _sym_) -> [ll-booleanp]
;; - _sym_ : [ll-symbolp]
;;
;; Returns `t` if _sym_ has a value bound to it, `nil` otherwise.
;;
;; AutoLISP built-in.

;; ### set
;; (**set** _sym_ _v_) -> [ll-anyp]
;; - _sym_ : [integerp]
;; - _v_ : [ll-anyp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### setq
;; (**setq** _sym_ _v_ [_sym_ _v_] ...) -> [ll-anyp]
;; - _sym_ : [integerp]
;; - _v_ : [ll-anyp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### quote
;; (**quote** _expr_)
;;
;; TODO
;;
;; AutoLISP built-in.


(if ll-features (ll-provide "call/def"))

;; Exports
'(
  ;defun
  ;defun-q
  ;lambda
  ;apply
  ;function
  ll-functionp
  ;boundp
  ;set setq
  ;quote
  )
