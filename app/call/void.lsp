;;; call/void.lsp --- Void Pseudo-Datatype

;; Copyright (C) CALL Team and Contributors

;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; # Void Pseudo-Datatype
;; ======================

;; ### ll-void
;; [ll-voidp]: **ll-void**
;;
;; Contains *void* value.
(setq
  ;; BricsCAD:
  ;;   (type (princ)) => SYM
  ;;   (vl-symbol-name (princ)) => "#<UNBOUND-VALUE>"
  ;; CorelCAD:
  ;;   (type (princ)) => NOTHING
  ;;   (type (type (princ))) => SYM
  ll-void (princ))

;; ### ll-voidp
;; (**ll-voidp** [ll-anyp]: _v_) -> [ll-booleanp]
;;
;; Returns `t` if _v_ is a *void*, `nil` otherwise.
(defun ll-voidp (v)
  (eq v ll-void))


(if ll-features (ll-provide "call/void"))

;; Exports
'(
  ll-void
  ll-voidp
  )
