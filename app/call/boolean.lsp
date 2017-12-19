;;; call/boolean.lsp --- Boolean Pseudo-Datatype

;; Copyright (C) CALL Team and Contributors

;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; # Boolean Pseudo-Datatype
;; =========================

;; ### ll-true
;; [ll-booleanp]: **ll-true**
;;
;; Contains `t`.
(setq ll-true (and))

;; ### ll-false
;; [ll-booleanp]: **ll-false**
;;
;; Contains `nil`.
(setq ll-false (or))

;; ### ll-booleanp
;; (**ll-booleanp** _v_): [ll-booleanp]
;; - _v_ : [ll-anyp]
;;
;; Returns `t` if _v_ contains a *boolean* value, i.e. `eq` to `t` or `nil`,
;; `nil` otherwise.
(defun ll-booleanp (v)
  (or (eq v ll-true)
      (eq v ll-false)))


(if ll-features (ll-provide "call/boolean"))

;; Exports
'(
  ll-true ll-false
  ll-booleanp
  )
