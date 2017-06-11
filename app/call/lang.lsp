;;; call/lang.lsp

;; Copyright (C) CALL Team and Contributors

;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; ## Evaluation
;; -------------

;; ### eval
;; (**eval** _expr_)
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

;; ### setq
;; (**setq** [ll-symbolp]: [ll-anyp]: _sym_ _v_ ...) -> [ll-anyp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### lambda
;; (**lambda** ...) -> [ll-functionp]
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

;; ### apply
;; (**apply** _fn_ _lst_) -> [ll-anyp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### defun
;; (**defun** ...) -> [ll-functionp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; defun-q
;; TODO

;; ### ll-functionp
;; (**ll-functionp** [ll-anyp]: _v_) -> [ll-booleanp]
;;
;; TODO
(defun ll-functionp (v)
  (or (member (type v) '(SUBR USUBR EXSUBR EXRXSUBR))))


;; ## Conditions and Flow Control
;; ------------------------------

;; ### cond
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### if
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### and
;; (**and** [_v_ ...]) -> [ll-booleanp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### or
;; (**or** [_v_ ...]) -> [ll-booleanp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### not
;; (**not** [ll-anyp]: _v_) -> [ll-booleanp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### ll-nand
;; (**ll-nand** [ll-anyp]: _v1_ [ll-anyp]: _v2_]) -> [ll-booleanp]
;;
;; TODO
(defun ll-nand (v1 v2)
  (not (and v1 v2)))

;; ### ll-nor
;; (**ll-nor** [ll-anyp]: _v1_ [ll-anyp]: _v2_]) -> [ll-booleanp]
;;
;; TODO
(defun ll-nor (v1 v2)
  (not (or v1 v2)))

;; ### ll-xor
;; (**ll-xor** [ll-anyp]: _v1_ [ll-anyp]: _v2_]) -> [ll-booleanp]
;;
;; TODO
(defun ll-xor (v1 v2)
  (not (eq (or v1) (or v2))))

;; ### progn
;; (**progn** ...) -> [ll-anyp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### exit, quit {exit}{quit}
;; (**exit**)
;; (**quit**)
;;
;; TODO
;;
;; AutoLISP built-in.


;; ## Iteration
;; ------------

;; ### repeat
;; (**repeat** [ll-integerp]: n ...) -> [ll-anyp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### while
;; (**while** ...) -> [ll-anyp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### foreach
;; (**foreach** [symbolp]: _sym_ [listp]: _lst_ ...) -> [ll-anyp]
;;
;; TODO
;;
;; AutoLISP built-in.


;; ## Equality and Comparsions
;; ---------------------------

;; ### equal
;; (**equal** [ll-anyp]: _v1_ [ll-anyp]: _v2_) -> [ll-booleanp]
;; (**equal** [ll-anyp]: _v1_ [ll-anyp]: _v2_ [numberp]: _fuzz_) -> [ll-booleanp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### eq
;; (**eq** [ll-anyp]: _v1_ [ll-anyp]: _v2_) -> [ll-booleanp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### = {#equ}
;; (**=**  _v_ ...) ->[ll-booleanp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### /= {#neq}
;; (**/=** _v_ ...) -> [ll-booleanp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### < {#lss}
;; (**<**  _v_ ...) -> [ll-booleanp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### <= {#leq}
;; (**<=** _v_ ...) -> [ll-booleanp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### > {#grt}
;; (**>**  _v_ ...) -> [ll-booleanp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### >= {#geq}
;; (**>=** _v_ ...) -> [ll-booleanp]
;;
;; TODO
;;
;; AutoLISP built-in.


;; Type
;; ====

;; ### type
;; (**type** [ll-anyp]: _v_) -> [ll-symbolp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### ll-typep
;; (**ll-typep** [ll-anyp]: _v_ [listp]: _typespec_) -> [ll-symbolp]
;;
;; TODO
(defun ll-typep (v typespec) ; FIXME: read Common Lisp docs
  (if (listp typespec)
    (and (member (type v) typespec))
    (eq (type v) typespec)))

;; ### ll-anyp
;; (**ll-anyp** [ll-anyp]: _v_) -> t
;;
;; Returns `t` for any type of LISP object.
(defun ll-anyp (v)
  t)


(if ll-features (ll-provide "call/lang"))

;; Exports
'(
  ;;
  ;; eval
  ;; quote
  ;; setq
  ;; lambda
  ;; function
  ;; apply
  ;; defun
  ;; defun-q
  ll-functionp
  ;;
  ;; cond if
  ;; and or
  ;; not
  ll-nand ll-nor
  ll-xor
  ;; progn
  ;; exit quit
  ;;
  ;; repeat while foreach
  ;;
  ;; equal eq
  ;; = /= < > <= >=
  ;;
  ;; type
  ll-typep
  ll-anyp
  )
