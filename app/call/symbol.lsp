;;; call/symbol.lsp --- Symbols

;; Copyright (C) CALL Team and Contributors

;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; # Symbols
;; =========

;; ### set
;; (**set** _sym_ _v_) -> [ll-anyp]
;; - _sym_ : [integerp]
;; - _v_ : [ll-anyp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### boundp
;; (**boundp** _sym_) -> [ll-booleanp]
;; - _sym_ : [ll-symbolp]
;;
;; Returns `t` if _sym_ has a value bound to it, `nil` otherwise.
;;
;; AutoLISP built-in.

;; ### ll-symbolp
;; (**ll-symbolp** [ll-anyp]: _v_) -> [ll-booleanp]
;;
;; Returns `t` if _v_ is a *symbol*, `nil` otherwise.
;;
;; Identical to VisualLISP `vl-symbolp`.
(if vl-symbolp
  (setq ll-symbolp vl-symbolp)
  (defun ll-symbolp (v)
    (eq 'SYM (type v))))

;; ### ll-symbol-value
;; (**ll-symbol-value** [ll-symbolp]: _sym_) -> [ll-anyp]
;;
;; TODO
;;
;; Identical to VisualLISP `vl-symbol-value`.
(if (and (not *call:ignore-vlisp*)
         vl-symbol-value)
    (setq ll-symbol-value vl-symbol-value)
    (defun ll-symbol-value (sym)
      (or (ll-symbolp sym) (set sym nil)) ; raise error if not symbolp
      (eval sym)))

;; ### ll-symbol-name
;; (**ll-symbol-name** [ll-symbolp]: _sym_) -> [ll-stringp]
;;
;; TODO
;;
;; Identical to VisualLISP `vl-symbol-name`.
(if (and (not *call:ignore-vlisp*)
         vl-symbol-name)
    (setq ll-symbol-name vl-symbol-name)
    (defun ll-symbol-name (sym / bound syms names)
      ;; FIXME: (ll-symbol-name 'names) gives nil
      (or (ll-symbolp sym) (set sym nil)) ; raise error if not symbolp
      (if (boundp sym)
          (setq bound t)
          (set sym t)) ; explicitly define symbol
      (setq syms t  ; preserve space \
            names t ; in atoms-family
            syms (atoms-family 0)
            names (atoms-family 1))
      (while (and syms
                  (not (eq sym (car syms))))
        (setq syms (cdr syms)
              names (cdr names)))
      (or bound
          (set sym nil)) ; undefine symbol
      (car names)))

;; ### atoms-family
;; (**atoms-family** [integerp]: _form_ [[listp]: _syms_]) -> [listp]
;;
;; Returns a list of the currently defined symbols.
;;
;; An integer _form_ detemines the format in which atoms-family returns the
;; symbol names:
;;   * 0 Return the symbol names as a list
;;   * 1 Return the symbol names as a list of strings
;;
;; If you specify _syms_, then `atoms-family` returns the specified symbols
;; that are currently defined, and returns `nil` for those symbols that are not
;; defined.
;;
;; AutoLISP built-in.


(if ll-features (ll-provide "call/symbol"))

;; Exports
'(
  ;; set
  ;; boundp
  ll-symbolp
  ll-symbol-value ll-symbol-name
  ;atoms-family
  )
