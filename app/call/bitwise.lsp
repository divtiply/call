;;; call/bitwise.lsp --- Bitwise Operations

;; Copyright (C) CALL Team and Contributors

;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; http://clhs.lisp.se/Body/f_bt_and.htm
;; http://clhs.lisp.se/Body/f_boole.htm
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Bitwise-Operations.html


;; # Bitwise Operations
;; ====================

;; ### ~ {bitwise-not}
;; > (**~** [ll-integerp]: _i_) -> [ll-integerp]
;;
;; Returns the bitwise NOT (1's complement) of _i_.
;;
;; AutoLISP built-in.

;; ### lsh
;; > (**lsh** [ll-integerp]: _k_ [ll-integerp]: _i_) -> [ll-integerp]
;;
;; Returns the logical bitwise shift of _k_ to the left _i_ bits, or to the
;; right if _i_ is negative.
;;
;; AutoLISP built-in.

;; TODO: ll-ash

;; ### logand
;; > (**logand** [ll-integerp]: _i1_ [ll-integerp]: _i2_ ...) -> [ll-integerp]
;;
;; Returns the result of the logical bitwise AND of its arfuments.
;;
;; AutoLISP built-in.

;; ### logior
;; > (**logior** [ll-integerp]: _i1_ [ll-integerp]: _i2_ ...) -> [ll-integerp]
;;
;; Returns the result of the logical bitwise inclusive OR of its arfuments.
;;
;; AutoLISP built-in.

;; ### boole
;; > (**boole** [ll-integerp]: _op_ [ll-integerp]: _i1_ [ll-integerp]: _i2_ ...) -> [ll-integerp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### ll-boole-OP {ll-boole-1}{ll-boole-2}{ll-boole-and}{ll-boole-andc1}{ll-boole-andc2}{ll-boole-c1}{ll-boole-c2}{ll-boole-clr}{ll-boole-eqv}{ll-boole-ior}{ll-boole-nand}{ll-boole-nor}{ll-boole-orc1}{ll-boole-orc2}{ll-boole-set}{ll-boole-xor}
;;
;; TODO
;;
;; Op              Result
;; ll-boole-1      integer-1
;; ll-boole-2      integer-2
;; ll-boole-andc1  and complement of integer-1 with integer-2
;; ll-boole-andc2  and integer-1 with complement of integer-2
;; ll-boole-and    and
;; ll-boole-c1     complement of integer-1
;; ll-boole-c2     complement of integer-2
;; ll-boole-clr    always 0 (all zero bits)
;; ll-boole-eqv    equivalence (exclusive nor)
;; ll-boole-ior    inclusive or
;; ll-boole-nand   not-and
;; ll-boole-nor    not-or
;; ll-boole-orc1   or complement of integer-1 with integer-2
;; ll-boole-orc2   or integer-1 with complement of integer-2
;; ll-boole-set    always -1 (all one bits)
;; ll-boole-xor    exclusive or
(setq
  ;; http://www.afralisp.net/archive/lisp/binary_II.htm
  ll-boole-1 3
  ll-boole-2 5
  ll-boole-and 1
  ll-boole-andc1 4
  ll-boole-andc2 2
  ll-boole-c1 12
  ll-boole-c2 10
  ll-boole-clr 0
  ll-boole-eqv 9
  ll-boole-ior 7
  ll-boole-nand 14
  ll-boole-nor 8
  ll-boole-orc1 13
  ll-boole-orc2 11
  ll-boole-set 15
  ll-boole-xor 6
  )


(if ll-features (ll-provide "call/bitwise"))

;; Exports
'(
  ;; ~
  ;; lsh
  ;; ll-ash
  ;; boole

  ;; Constants:
  ll-boole-1
  ll-boole-2
  ll-boole-and
  ll-boole-andc1
  ll-boole-andc2
  ll-boole-c1
  ll-boole-c2
  ll-boole-clr
  ll-boole-eqv
  ll-boole-ior
  ll-boole-nand
  ll-boole-nor
  ll-boole-orc1
  ll-boole-orc2
  ll-boole-set
  ll-boole-xor
  )
