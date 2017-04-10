;;; call/type.lsp ---

;; Copyright (C) CALL Team and Contributors

;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; ### ll-anyp
;; (**ll-anyp** [ll-anyp]: _v_) -> t
;;
;; Returns `t` for any type of LISP object.
(defun ll-anyp (v)
  t)

;; ### type
;; (**type** _v_) -> [ll-symbolp]
;; - _v_ : [ll-anyp]
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


(if ll-features (ll-provide "call/type"))

;; Exports
'(
  ll-anyp
  ;type
  ll-typep
  )
