;;; call/lsets.lsp --- Lists as Sets

;; Copyright (C) CALL Team and Contributors

;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; TODO: Performance improvements:
;; * Convert `foreach` loops to `while` loops.


;; # Lists as Sets
;; ===============

;; ll--insetp == ll-inlistp
(cond
  ((and (not *call:ignore-lispex*)
        vle-member)
   (setq ll--insetp vle-member))
  ((and (not *call:ignore-vlisp*)
        vl-position)
   (defun ll--insetp (v lst)
     (or (vl-position v lst))))
  (t
   (defun ll--insetp (v lst)
     (or (member v lst)))))

;; ### ll-subsetp
;; (**ll-subsetp** [listp]: _lst1_ [listp]: _lst2_) -> [ll-booleanp]
;;
;; TODO
(defun ll-subsetp (lst1 lst2)
  (while (and lst1
              (ll--insetp (car lst1) lst2))
    (setq lst1 (cdr lst1)))
  (null lst1))

;; ### ll-adjoin
;; (**ll-adjoin** [ll-anyp]: _v_ [listp]: _lst_) -> [listp]
;;
;; TODO
(defun ll-adjoin (v lst)
  (if (ll--insetp v lst)
      lst
      (cons v lst)))

;; ### ll-union
;; (**ll-union** [listp]: _lst1_ [listp]: _lst2_) -> [listp]
;;
;; TODO
;;
;; Identical to LispEx `vle-list-union`.
(if (and (not *call:ignore-lispex*)
         vle-list-union)
    (setq ll-union vle-list-union)
    (defun ll-union (lst1 lst2)
      (foreach x lst2
        (or (ll--insetp x lst1)
            (setq lst1 (cons x lst1))))
      lst1))

;; ### ll-intersection
;; (**ll-intersection** [listp]: _lst1_ [listp]: _lst2_) -> [listp]
;;
;; TODO
;;
;; Identical to LispEx `vle-list-intersect`.
(if (and (not *call:ignore-lispex*)
         vle-list-intersect)
    (setq ll-intersection vle-list-intersect)
    (defun ll-intersection (lst1 lst2 / out)
      ;; (vl-remove-if-not '(lambda (x) (member x l2)) l1)
      (foreach x lst2
        (if (ll--insetp x lst1)
            (setq out (cons x out))))
      out))

;; ### ll-set-difference
;; (**ll-set-difference** [listp]: _lst1_ [listp]: _lst2_) -> [listp]
;;
;; TODO
;;
;; Identical to LispEx `vle-list-diff`.
(if (and (not *call:ignore-lispex*)
         vle-list-diff)
    (setq ll-set-difference vle-list-diff)
    (defun ll-set-difference (lst1 lst2 / out)
      ;; (vl-remove-if '(lambda (x) (member x lst2)) lst1)
      (foreach x lst2
        (or (ll--insetp x lst1)
            (setq out (cons x out))))
      out))

;; ### ll-set-exclusive-or
;; (**ll-set-exclusive-or** [listp]: _lst1_ [listp]: _lst2_) -> [listp]
;;
;; TODO
;;
;; Identical to LispEx `vle-list-subtract`.
(if (and (not *call:ignore-lispex*)
         vle-list-subtract)
    (setq ll-set-exclusive-or vle-list-subtract)
    (defun ll-set-exclusive-or (lst1 lst2)
      (append (ll-set-difference lst1 lst2)
              (ll-set-difference lst2 lst1))))


(if ll-features (ll-provide "call/lsets"))

;; Exports
'(
  ;; ll--insetp
  ll-subsetp ll-adjoin
  ll-union ll-intersection ll-set-difference ll-set-exclusive-or
  )
