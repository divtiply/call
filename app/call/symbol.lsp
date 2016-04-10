;;; symbol.lsp --- Symbol Functions

;; Copyright (c) CALL Team and Сontributors
;;
;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; # Symbols
;; =========


;; ## Symbol Predictates
;; ---------------------

;; ### boundp
;; (**boundp** _sym_) -> [cl-booleanp]
;; - _sym_ : [cl-symbolp]
;;
;; Returns `t` if _sym_ has a value bound to it, `nil` otherwise.
;;
;; AutoLISP built-in.

;; ### cl-symbolp
;; (**cl-symbolp** _v_) -> [cl-booleanp]
;; - _v_ : [cl-objectp]
;;
;; Returns `t` if _v_ is a *symbol*, `nil` otherwise.
;;
;; Identical to VisualLisp `vl-symbolp`.
(if vl-symbolp
  (setq cl-symbolp vl-symbolp)
  (defun cl-symbolp (v)
    (eq 'SYM (type v))))

;; #### cl-objectp
;; (**cl-objectp** _v_) -> t
;; - _v_ : [cl-objectp]
;;
;; Returns `t` for any LISP object.
(defun cl-objectp (v)
  t)

;; ### cl-booleanp
;; (**cl-booleanp** _v_) -> [cl-booleanp]
;; - _v_ : [cl-objectp]
;;
;; Returns `t` if _v_ is a *boolean*, i.e. `eq` to `t` or `nil`,
;; `nil` otherwise.
(defun cl-booleanp (v)
  (or (eq v t)
      (eq v nil)))

;; #### cl-voidp
;; (**cl-voidp** _v_) -> [cl-booleanp]
;; - _v_ : [cl-objectp]
;;
;; Returns `t` if _v_ is a *void*, `nil` otherwise.
(defun cl-voidp (v)
  (eq v (cl-void)))


;; ## Symbol Functions
;; -------------------

;; ### quote
;; (**quote** _expr_)
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### set
;; (**set** _sym_ _v_) -> [cl-objectp]
;; - _sym_ : [integerp]
;; - _v_ : [cl-objectp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### setq
;; (**setq** _sym_ _v_ [_sym_ _v_] ...) -> [cl-objectp]
;; - _sym_ : [integerp]
;; - _v_ : [cl-objectp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### atoms-family
;; (**atoms-family** _form_ [_syms_]) -> [listp]
;; - _form_ : [integerp]
;; - _syms_ : [listp]
;;
;; Returns a list of currently defined symbols.
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

;; ### type
;; (**type** _v_) -> [cl-symbolp]
;; - _v_ : [cl-objectp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### cl-typep
;; (**cl-typep** _v_ _typespec_) -> [cl-symbolp]
;; - _v_ : [cl-objectp]
;; - _typespec_ : [listp]
;;
;; TODO
(defun cl-typep (v typespec) ; FIXME: read Common Lisp docs
  (if (listp typespec)
    (and (member (type v) type`spec))
    (eq (type v) typespec)))

;; ### cl-symbol-value
;; (**cl-symbol-value** _sym_) -> [cl-objectp]
;;
;; TODO
;;
;; Identical to VisualLisp `vl-symbol-value`.
(if (and (not *call:ignore-vlisp*)
         (boundp vl-symbol-value))
    (setq cl-symbol-value vl-symbol-value)
    (defun cl-symbol-value (sym)
      (or (cl-symbolp sym) (set sym nil)) ; raise error if not symbolp
      (eval sym)))

;; ### cl-symbol-name
;; (**cl-symbol-name** _sym_) -> [cl-stringp]
;; - _sym_ : [cl-symbolp]
;;
;; TODO
;;
;; Identical to VisualLisp `vl-symbol-name`.
(if (and (not *call:ignore-vlisp*)
         (boundp vl-symbol-name))
    (setq cl-symbol-name vl-symbol-name)
    (defun cl-symbol-name (sym / bound syms names)
      ;; FIXME: (cl-symbol-name 'names) gives nil
      (or (cl-symbolp sym) (set sym nil)) ; raise error if not symbolp
      (if (boundp sym)
          (setq bound t)
          (set sym t)) ; explicitly define symbol
      (setq syms t  ; preserve space \
            names t ; in atoms-family
            syms (atoms-family 0)
            names (atoms-family 1))
; debug: (print (nth (vl-position sym syms) names))
      (while (and syms
                  (not (eq sym (car syms))))
        (setq syms (cdr syms)
              names (cdr names)))
      (or bound
          (set sym nil)) ; undefine symbol
      (car names)))

;; (defun cl-symbol-name (sym / bound out)
;;   (or (eq 'SYM (type sym)) (set sym nil)) ; raise error if not symbolp
;;   (if (boundp sym)
;;       (setq bound t)
;;       (set sym t)) ; explicitly define symbol
;;   (setq out t  ; preserve space in atoms-family
;;         out (cdr (assoc sym (mapcar (function cons)
;;                                     (atoms-family 0)
;;                                     (atoms-family 1)))))
;;   (or bound
;;       (set sym nil)) ; undefine symbol
;;   out)

; (defun cl-true () t)
; (defun cl-false () nil)

;; ### cl-void
;; (**cl-void**) -> [cl-voidp]
;;
;; Returns *void* value.
;;
;; Equivalent to `(princ)`.
(defun cl-void ()
  (princ))


;; Exports
'(
  ;; boundp
  cl-symbolp
  cl-objectp
  cl-booleanp
  cl-voidp
  ;; quote
  ;; set setq
  ;; atoms-family
  ;; type
  cl-typep
  cl-symbol-value cl-symbol-name
  ;; cl-true cl-false
  cl-void
  )
