;;; call/lists.lsp --- Lists

;; Copyright (C) CALL Team and Contributors

;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; # Lists


;; ## List Predictates

;; ### (**atom** _v_: [ll-anyp]): [ll-booleanp]
;;
;; Returns `t` if _v_ is an *atom*, `nil` otherwise.
;;
;; All objects except *cons* cells are atoms. The symbol `nil` is an atom and
;; is also a list; it is the only Lisp object that is both.
;;
;; AutoLISP built-in.

;; ### (**null** _v_: [ll-anyp]): [ll-booleanp]
;;
;; Returns `t` if _v_ is `nil`, `nil` otherwise.
;;
;; This function is identical to `not`, but as a matter of clarity we use
;; `null` when object is considered a list and `not` when it is considered a
;; truth value.
;;
;; AutoLISP built-in.

;; ### (**ll-consp** _v_: [ll-anyp]): [ll-booleanp]
;;
;; Returns `t` if _v_ is a *cons* cell, `nil` otherwise.
;;
;; `nil` is not a cons cell, although it is a list.
;;
;;     (ll-consp nil) => NIL
;;     (ll-consp (cons 1 2)) => T
;;
;; Equivalent to `(not (atom v))`.
;; Identical to VisualLISP `vl-consp`.
(if (and (not *call:ignore-vlisp*)
         vl-consp)
    (setq ll-consp vl-consp)
    (defun ll-consp (v)
      (not (atom v)))) ; (eq 'LIST (type v))

;; ### (**listp** _v_: [ll-anyp]): [ll-booleanp]
;;
;; Returns `t` if _v_ is a *cons* cell or `nil`, `nil` otherwise.
;;
;;     (listp nil) => T
;;     (listp (cons 1 2)) => T
;;
;; AutoLISP built-in.

;; ### (**ll-endp** _x_: [listp]): [ll-booleanp]
;;
;; Like `null`, but signals an error if _x_ is not a *list*.
(defun ll-endp (x)
  (car x) ; raise error if not listp
  (null x))


;; ## List Constructors

;; ### (**cons** _a_: [ll-anyp] _d_: [ll-anyp]): [ll-consp]
;;
;; Returns a newly allocated *cons* cell whose first element is _a_ and second
;; element is _d_.
;;
;;     (cons 1 2) => (1 . 2)
;;     (cons 1 '()) => (1)
;;
;; AutoLISP built-in.

;; ### (**list** _v_: [ll-anyp] ...): [listp]
;;
;; Returns a newly allocated list containing the _v_s as its elements.
;;
;;     (list 1 2 3 4) => (1 2 3 4)
;;     (list (list 1 2) (list 3 4)) => ((1 2) (3 4))
;;     (list) => NIL
;;
;; AutoLISP built-in.

;; ### (**ll-make-list** _n_: [ll-natnump] _v_: [ll-anyp]): [listp]
;;
;; Returns a list of _n_ elements, in wich each element is _v_.
;;
;;     (ll-make-list 7 'foo) => (FOO FOO FOO FOO FOO FOO FOO)
;;
(defun ll-make-list (n v / x)
  (repeat n
    (setq x (cons v x))))

;; ### (**ll-iota** _n_: [ll-natnump]): [listp]
;;
;; TODO
(defun ll-iota (n / x)
  ;; same as (ll-iota-range 0 n 1)
  (repeat n
    (setq x (cons (setq n (1- n)) x))))

;; (defun ll-range (start end step / len x)
;;   ;; FIXME: try (ll-range 0 10.1 0.5)
;;   (setq len (- end start)
;;         len (fix (if (zerop (rem len step))
;;                      (/ len step)
;;                      (1+ (/ len step)))
;;         end (+ start (* step len)))
;;   (repeat
;;     (setq x (cons (setq end (- end step)) x))))


;; ## List Selectors

;; ### (**car** _p_: [ll-consp]): [ll-anyp]
;;
;; Returns the value referred to by the first slot of the cons cell _p_.
;; In other words, it returns the car of _p_.
;;
;; As a special case, if _p_ is nil, this function returns `nil`.
;; Therefore, any list is a valid argument. An error is signaled if the
;; argument is not a cons cell or `nil`.
;;
;;     (car '(a b c)) => A
;;     (car '()) => NIL
;;
;; AutoLISP built-in.

;; ### (**cdr** _p_: [ll-consp]): [ll-anyp]
;;
;; Returns the value referred to by the second slot of the cons cell _p_.
;; In other words, it returns the cdr of _p_.
;;
;; As a special case, if _p_ is `nil`, this function returns `nil`.
;; Therefore, any list is a valid argument. An error is signaled if the
;; argument is not a cons cell or `nil`.
;;
;;     (cdr '(a b c)) => (B C)
;;     (cdr '()) => NIL
;;
;; AutoLISP built-in.

;; ### (**cddddr** _p_: [ll-consp]): [ll-anyp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### (**ll-first** _x_: [listp]): [ll-anyp]
;;
;; Alias for `car`.
;;
;; Likewise, the functions `ll-second`, `ll-third`, `ll-fourth`, `ll-fifth`,
;; `ll-sixth`, `ll-seventh`, `ll-eighth`, `ll-ninth` and `ll-tenth` return the
;; given element of the _x_.
;;
;; Identical to BricsCAD LISP `vle-nth0` (`ll-first`) ... `vle-nth9` (`ll-tenth`).
(if (and (not *call:ignore-lispex*)
         vle-nth0 vle-nth1 vle-nth2 vle-nth3 vle-nth4
         vle-nth5 vle-nth6 vle-nth7 vle-nth8 vle-nth9)
    (setq
      ll-first   vle-nth0
      ll-second  vle-nth1
      ll-third   vle-nth2
      ll-fourth  vle-nth3
      ll-fifth   vle-nth4
      ll-sixth   vle-nth5
      ll-seventh vle-nth6
      ll-eighth  vle-nth7
      ll-ninth   vle-nth8
      ll-tenth   vle-nth9
      )
    (progn
      (setq
        ll-first   car
        ll-second  cadr
        ll-third   caddr
        ll-fourth  cadddr
        )
      (defun ll-fifth   (x)       (car    (cddddr x)))
      (defun ll-sixth   (x)       (cadr   (cddddr x)))
      (defun ll-seventh (x)       (caddr  (cddddr x)))
      (defun ll-eighth  (x)       (cadddr (cddddr x)))
      (defun ll-ninth   (x) (car  (cddddr (cddddr x))))
      (defun ll-tenth   (x) (cadr (cddddr (cddddr x))))
      ))

;; ### (**ll-rest** _x_: [listp]): [ll-anyp]
;;
;; Alias for `cdr`.
(setq ll-rest cdr)

;; ### (**last** _x_: [listp]): [ll-anyp]
;;
;; Returns the last element of _x_.
;;
;; If _x_ is null, `nil` is returned.
;;
;;     (last '(a b c)) => C
;;
;; AutoLISP built-in.
;;
;; See also `ll-last-cons`, `ll-lastn`.

;; ### (**nth** _n_: [ll-natnump] _x_: [listp]): [ll-anyp]
;;
;; Returns the _n_-th element of _x_.
;;
;; Elements are numbered starting with zero, so the car of list is element
;; number zero. If the length of _x_ is _n_ or less, the value is `nil`.
;;
;;     (nth 2 '(1 2 3 4)) => 3
;;     (nth 10 '(1 2 3 4)) => NIL
;;
;; Equivalent to `(car (ll-nthcdr n list))`.
;;
;; AutoLISP built-in.

;; ### (**ll-nthcdr** _n_: [ll-natnump] _x_: [listp]): [ll-anyp]
;;
;; Returns the _n_-th cdr of _x_. In other words, it skips past the first
;; _n_ links of list and returns what follows.
;;
;; If _n_ is zero, returns all of list. If the length of _x_ is _n_ or less,
;; returns `nil`.
;;
;;     (ll-nthcdr 1 '(1 2 3 4)) => (2 3 4)
;;     (ll-nthcdr 9 '(1 2 3 4)) => NIL
;;
(if (and (not *call:ignore-lispex*)
         vle-sublist)
    (defun ll-nthcdr (n x)
      (vle-sublist x n 0))
    (defun ll-nthcdr (n x)
      (nth n (list nil)) ; raise error if not natnump
      (while (and x (< 0 n))
        (setq n (1- n)
              x (cdr x)))
      x))

;; ### (**ll-firstn** _x_: [listp] _n_: [ll-natnump]): [listp]
;;
;; Returns first _n_ elements of _x_.
;;
;; If _x_ is null, `nil` is returned.
(if (and (not *call:ignore-lispex*)
         vle-sublist)
    (defun ll-firstn (x n)
      (if (/= 0 n)
          (vle-sublist x 0 n)))
    (defun ll-firstn (x n / y)
      (nth n (list nil)) ; raise error if not natnump
      (while (and x (< 0 n))
        (setq n (1- n)
              y (cons (car x) y)
              x (cdr x)))
      (reverse y)))

;; ### (**ll-butlast** _x_: [listp]): [listp]
;;
;; Returns a copy of _x_ with the last element removed.
;;
;; Identical to BricsCAD LISP `vle-remove-last`.
(if (and (not *call:ignore-lispex*)
         vle-remove-last)
    (setq ll-butlast vle-remove-last)
    (defun ll-butlast (x)
      (reverse (cdr (reverse x)))))

;; ### (**ll-butlastn** _x_: [listp] _n_: [ll-natnump]): [listp]
;;
;; Returns a copy of _x_ with the last _n_ elements removed.
;;
;; FIXME: If _n_ is greater than zero it makes a copy of the list so as not to
;; damage the original list. In general, `(append (ll-butlastn x n) (ll-lastn
;; x n))` will return a list equal to _x_.
(if (and (not *call:ignore-lispex*)
         vle-sublist)
    (defun ll-butlastn (x n / len)
      (if (< n (setq len (length x)))
          (vle-sublist x 0 (- len n))))
    (defun ll-butlastn (x n)
      (reverse (ll-nthcdr n (reverse x)))))

;; ### (**ll-last-cons** _x_: [listp]): [ll-consp]
;;
;; Returns the last cons of _x_.
;;
;;     (ll-last-cons '(a b c)) => (C)
;;     (ll-last-cons (cons a b)) => B
(defun ll-last-cons (x)
  ;; (while (ll-consp (cdr x))
  (while (not (atom (cdr x)))
    (setq x (cdr x)))
  x)

;; ### (**ll-lastn** _x_: [listp] _n_: [ll-natnump]): [listp]
;;
;; Returns the last _n_ conses (not the last _n_ elements) of _x_.
;;
;; If _x_ is null, `nil` is returned. If _n_ is non-`nil`, the
;; _n_-th-to-last element is returned instead, or the whole of list if _n_ is
;; bigger than list's length.
(if (and (not *call:ignore-lispex*)
         vle-sublist)
    (defun ll-lastn (x n)
      (vle-sublist x (- (length x) n) 0))
    ;; Faster for larger Ns.
    (defun ll-lastn (x n / y)
      (setq y x
            x (ll-nthcdr n x))
      (while x
        (setq y (cdr y)
              x (cdr x)))
      y))
    ;; Faster for smaller Ns.
    ;; (defun ll-lastn (x n / y)
    ;;   ;; Non-conformity: returns a _copy_ of the list tail,
    ;;   ;; should return the tail itself.
    ;;   (nth n (list nil)) ; raise error if not natnump
    ;;   (setq x (reverse x))
    ;;   (while (and x (< 0 n))
    ;;     (setq n (1- n)
    ;;           y (cons (car x) y)
    ;;           x (cdr x)))
    ;;   y)

;; ### (**ll-sublist** _x_: [listp] _start_: [ll-natnump] _len_: [ll-natnump]): [listp]
;;
;; TODO: Returns the sublist of _x_ starting with item at position _start_.
;; TODO: If _len_ <= 0 process to end of the list.
;;
;; FIXME: NOT EXACT ~~Equivalent to `(ll-firstn (ll-nthcdr start x) len)`.~~
;;
;; Identical to BricsCAD LISP `vle-sublist`.
(if (and (not *call:ignore-lispex*)
         vle-sublist)
    (setq ll-sublist vle-sublist)
    (defun ll-sublist (x start len)
      (if (< 0 len)
          (ll-firstn (ll-nthcdr start x) len)
          (ll-nthcdr start x))))

;; ### (**ll-subseq** _x_: [listp] _start_: [ll-natnump] _end_: [ll-natnump]): [listp]
;;
;; TODO
(defun ll-subseq (x start end)
  (ll-sublist x start (if end (- end start) 0))) ; FIXME: 0 is bad for firstn (see solution above)


;; ## List Splitting

;; ### (**ll-split-if** _pred_: [ll-functionp] _x_: [listp]): [listp]
;; ### (**ll-split-if-not** _pred_: [ll-functionp] _x_: [listp]): [listp]
;;
;; TODO
(if *call:enable-preeval*
  (progn
    (defun ll-split-if (pred x / y)
      (setq pred (eval pred))
      (while (and x
                  (not (pred (car x))))
        (setq y (cons (car x) y)
              x (cdr x)))
      (list (reverse y) x))
    (defun ll-split-if-not (pred x / y)
      (setq pred (eval pred))
      (while (and x
                  (pred (car x)))
        (setq y (cons (car x) y)
              x (cdr x)))
      (list (reverse y) x)))
  (progn
    (defun ll-split-if (pred x / y)
      (while (and x
                  (not (apply pred (list (car x)))))
        (setq y (cons (car x) y)
              x (cdr x)))
      (list (reverse y) x))
    (defun ll-split-if-not (pred x / y)
      (while (and x
                  (apply pred (list (car x))))
        (setq y (cons (car x) y)
              x (cdr x)))
      (list (reverse y) x))))

;; ### (**ll-split-at** _n_: [ll-natnump] _x_: [listp]): [listp]
;;
;; Returns a list of two sublists, where first sublist is first _n_ elements of
;; _x_, and second sublist is the remaining elements.
;;
;;     (ll-split-at 3 '(a b c d e f g)) => ((A B C) (D E F G))
;;
;;     (mapcar 'set '(head tail) (ll-split-at 3 '(1 2 3 4 5)))
;;       => ((1 2 3) (4 5))
;;     head
;;       => (1 2 3)
;;     tail
;;       => (4 5)
;;
;; Equivalent to `(list (ll-firstn x n) (ll-nthcdr n x))`, except that it can be
;; faster.
(if (and (not *call:ignore-lispex*)
         vle-sublist)
    (defun ll-split-at (n x)
      (if (< 0 n)
          (list (vle-sublist x 0 n) (vle-sublist x n 0))
          (list nil x)))
    (defun ll-split-at (n x / y)
      (nth n (list nil)) ; raise error if not natnump
      (while (and x (< 0 n))
        (setq n (1- n)
              y (cons (car x) y)
              x (cdr x)))
      (list (reverse y) x)))

;; ### (**ll-split-at-first** _v_: [ll-anyp] _x_: [listp]): [listp]
;;
;; TODO
;;
;; Identical to BricsCAD LISP `vle-list-split`.
;; Similar to Express Tools `acet-list-split`, but with reverse argument
;; order.
(if (and (not *call:ignore-lispex*)
         vle-list-split)
    (setq ll-split-at-first vle-list-split)
    (defun ll-split-at-first (v x / y)
      (while (and x
                  (not (equal v (car x))))
        (setq y (cons (car x) y)
              x (cdr x)))
      (list (reverse y) x)))

;; ### (**ll-split-at-last** _v_: [ll-anyp] _x_: [listp]): [listp]
;;
;; TODO
(defun ll-split-at-last (v x / in y)
  ;; TODO: OPTIMIZE: search & save pointer, then search more
  (setq in (reverse x))
  (while (and in
              (not (equal v (car in))))
    (setq y (cons (car in) y)
          in (cdr in)))
  (if in
      (cons (reverse (cdr in)) (cons (car in) y)) ;---!!! FIXME
      (list x)))

;; ### (**ll-partition** _n_: [ll-natnump] _x_: [listp]): [listp]
;;
;; TODO
;;
;;     (ll-partition 2 '(a b c d e)) => ((a b)(c d))
;;
(defun ll-partition (n x) ; split-every, chunk
  (*error* "ll-partition: Function not implemented")) ;--- TODO

;; ### (**ll-partition-all** _n_: [ll-natnump] _x_: [listp]): [listp]
;;
;; TODO
;;
;;     (ll-partition-all 2 '(a b c d e)) => ((a b)(c d)(e))
;;
(defun ll-partition-all (n x)
  (*error* "ll-partition-all: Function not implemented")) ;--- TODO

;; ### (**ll-separate** _pred_: [ll-functionp] _x_: [listp]): [listp]
;;
;; TODO
;;
;;     (ll-separate 'numberp '(a b 1 c 2)) => ((1 2) (a b c))
;;
(if *call:enable-preeval*
    (defun ll-separate (pred x / y1 y2)
      (setq pred (eval pred))
      (foreach x x
        (if (pred x)
            (setq y1 (cons x y1))
            (setq y2 (cons x y2))))
      (list (reverse y1) (reverse y2)))
    (defun ll-separate (pred x / y1 y2)
      (foreach x x
        (if (apply pred (list x))
            (setq y1 (cons x y1))
            (setq y2 (cons x y2))))
      (list (reverse y1) (reverse y2))))


;; ## List Operations

;; ### (**length** _x_: [listp]): [ll-natump]
;;
;; Returns the number of elements in _x_.
;;
;; It is an error if _x_ is an improper list.
;;
;;     (length '(1 2 3)) => 3
;;     (length '()) => 0
;;
;; AutoLISP built-in.

;; ### ll-list-length
;; (ll-list-length _x_: [listp]): ([ll-natnump] [null])
;;
;; Returns the length of _x_, exactly like `length`, except that if _x_
;; is an improper list, `nil` is returned.
;;
;; Identical to VisualLISP `vl-list-length`.
(if (and (not *call:ignore-vlisp*)
         vl-list-length)
    (setq ll-list-length vl-list-length)
    (defun ll-list-length (x / len)
      (setq len 0)
      (while (and x
                  (listp (setq x (cdr x))))
        (setq len (1+ len)))
      (if (null x)
          len)))

;; ### (**reverse** _x_: [listp]): [listp]
;;
;; Returns a list that has the same elements as _x_, but in reverse order.
;;
;;     (reverse (list 1 2 3 4)) => (4 3 2 1)
;;
;; AutoLISP built-in.

;; ### (**append** _x_: [listp] ...): [listp]
;;
;; Returns a list that contains all of the elements of the given lists in
;; order.
;;
;;     (append '(x) '(y))        =>  (X Y)
;;     (append '(a) '(b c d))    =>  (A B C D)
;;     (append '(a (b)) '((c)))  =>  (A (B) (C))
;;
;; AutoLISP built-in.

;; ### (**ll-revappend** _x_: [listp] _v_: [ll-anyp]): [listp]
;;
;; TODO
;; If v is a list then (ll-revappend x v) == (append (reverse x) v)
;;
;;     (ll-revappend '(1 2 3) '()) => (3 2 1)
;;     (ll-revappend '(1 2 3) '(a . b)) => (3 2 1 A . B)
;;     (ll-revappend '() '(a b c)) => (A B C)
;;     (ll-revappend '(1 2 3) 'a) => (3 2 1 . A)
;;     (ll-revappend '() 'a) =>  A ; degenerate case
;;
(defun ll-revappend (x v)
  (while x
    (setq v (cons (car x) v)
          x (cdr x)))
  v)

;; ### (**ll-copy-list** _x_: [listp])
;;
;; TODO
(defun ll-copy-list (x)
  ;; http://www.gigamonkeys.com/book/beyond-lists-other-uses-for-cons-cells.html
  ;; FIXME: OPTIMIZE
  (if (atom x)
      x
      (cons (car x) (ll-copy-list (cdr x)))))
;; (defun ll-copy-list (x)
;;   ;; (append x nil)) ; Doesn't works properly in BircsCAD
;;   (append nil x nil)) ; FIXME: (LL-COPY-LIST '(A . B)) fails

;; TODO: copy-alist

;; ### (**ll-copy-tree** _x_: [listp])
;;
;; TODO
(defun ll-copy-tree (x)
  ;; http://www.gigamonkeys.com/book/beyond-lists-other-uses-for-cons-cells.html
  ;; FIXME: OPTIMIZE
  (if (atom x)
      x
      (cons (ll-copy-tree (car x))
        (ll-copy-tree (cdr x)))))

;; ### (**ll-count** _v_: [ll-anyp] _x_: [listp]): [ll-natnump]
;;
;; Returns the number of elements of _x_ which match _v_.
(defun ll-count (v x / n)
  (setq n 0)
  (while (setq x (member v x))
    (setq x (cdr x)
          n (1+ n)))
  n)

;; ### (**ll-count-if** _pred_: [ll-functionp] _x_: [listp]): [ll-natnump]
;; (**ll-count-if-not** _pred_: [ll-functionp] _x_: [listp]): [ll-natnump]
;;
;; TODO
(if *call:enable-preeval*
  (progn
    (defun ll-count-if (pred x / n)
      ;; TODO: rewrite using while (see count)
      (setq pred (eval pred)
            n 0)
      (foreach v x
        (if (pred v)
            (setq n (1+ n))))
      n)
    (defun ll-count-if-not (pred x / n)
      ;; TODO: rewrite using while (see count)
      (setq pred (eval pred)
            n 0)
      (foreach v x
        (or (pred v)
            (setq n (1+ n))))
      n))
  (progn
    (defun ll-count-if (pred x / n)
      ;; TODO: rewrite using while (see count)
      (setq n 0)
      (foreach v x
        (if (apply (function pred) (list v))
            (setq n (1+ n))))
      n))
    ;; TODO (defun count-if-not
    )


;; ## List Iteration

;; ### (**mapcar** _fn_: [ll-functionp] _x_: [listp] ...): [listp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### (**ll-maplist** _fn_: [ll-functionp] _x_: [listp]): [listp]
;;
;; TODO
(if *call:enable-preeval*
    (defun ll-maplist (fn x / y) ; TODO: should it support improper lists?
      (car x) ; raise error if not listp
      (setq fn (eval fn))
      (while x
        (setq y (cons (fn x) y)
              x (cdr x)))
      (reverse y))
    (defun ll-maplist (fn x / y) ; TODO: should it support improper lists?
      (car x) ; raise error if not listp
      (while x
        (setq y (cons (apply fn (list x)) y)
              x (cdr x)))
      (reverse y)))

;; ### (**ll-maptree** _fn_: [ll-functionp] _x_: [listp]): [listp]
;;
;; TODO
; FIXME: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; FIXME: (if *call:enable-preeval*
(defun ll-maptree (fn x / y pend cur pendlst newlst)
  ;; based on STDLIB std-mapatom
  ;; see ll-flatten
  (cond
    ((atom x) (apply fn (list x)))
    (t
     ;; loop until last element, for '(1 2 . 3) too
     (while (not (atom x)) ; (consp x)
       (setq pend (cons (car x) pend)
             x (cdr x)))
     (setq y (if x (apply fn (list x)))
           pendlst nil
           newlst nil)
     (while pend
       (while pend
         (setq cur (car pend) pend (cdr pend))
         (if (atom cur)
             (setq y (cons (apply fn (list cur)) y))
             (progn
               (setq pendlst (cons pend pendlst)
                     newlst (cons y newlst) pend nil)
               (while (not (atom cur)) ; (consp cur)
                 (setq pend (cons (car cur) pend) cur (cdr cur)))
               (setq y (if cur (apply fn (list cur)))))))
       (while (and pendlst (null pend))
         (setq y (cons y (car newlst)) newlst (cdr newlst)
               pend (car pendlst) pendlst (cdr pendlst))))
     y)))

;; ### (**ll-some** _pred_: [ll-functionp] _x_: [listp]): [ll-booleanp]
;;
;; TODO
;;
;; Similar to VisualLISP `vl-some`, but may accept only one list.
(if (and (not *call:ignore-vlisp*)
         vl-some)
    ;; NOTE: BricsCAD LISP allows improper list, VisualLISP not.
    (defun ll-some (pred x)
      (vl-some pred x))
    (defun ll-some (pred x)
      (and (ll-member-if pred x))))

;; ### (**ll-every** _pred_: [ll-functionp] _x_: [listp]): [ll-booleanp]
;;
;; TODO
;;
;; Similar to VisualLISP `vl-every`, but may accept only one list.
(if (and (not *call:ignore-vlisp*)
         vl-every)
    ;; NOTE: BricsCAD LISP allows improper list, VisualLISP not.
    (defun ll-every (pred x)
      (vl-every pred x))
    (defun ll-every (pred x)
      (not (ll-member-if-not pred x))))

;; ### (**ll-foldl** _fn_: [ll-functionp] _init_: [ll-anyp] _x_: [listp]): [listp]
;;
;; The fundamental list iterator.
;;
;; TODO
;; `ll-foldl` passes parameters to _fn_ in the same order as Scheme does,
;; i.e. `(fn current-item accumulator)`.
;;
;;     (ll-foldl 'cons '() '(1 2 3 4)) => (4 3 2 1)
;;     (ll-foldl 'list '() '(1 2 3 4)) => (4 (3 (2 (1 NIL))))
;;
;; See also `ll-foldr`, `ll-reduce`.
(if *call:enable-preeval*
    (defun ll-foldl (fn init x)
      (setq fn (eval fn))
      (while x
        (setq init (fn (car x) init) ; Scheme fn args order
              x (cdr x)))
      init)
    (defun ll-foldl (fn init x)
      (while x
        (setq init (apply fn (list (car x) init)) ; Scheme fn args order
              x (cdr x)))
      init))

;; ### (**ll-foldr** _fn_: [ll-functionp] _init_: [ll-anyp] _x_: [listp]): [listp]
;;
;; The fundamental list recursion iterator.
;;
;; TODO
;;
;;     (ll-foldr 'cons '() '(1 2 3 4)) => (1 2 3 4)
;;     (ll-foldr 'list '() '(1 2 3 4)) => (1 (2 (3 (4 NIL)))
;;
;; See also `ll-foldl`, `ll-reduce`.
(defun ll-foldr (fn init x)
  (ll-foldl fn init (reverse x)))

;; ### (**ll-reduce** _fn_: [ll-functionp] _x_: [listp]): [ll-anyp]
;; (**ll-reduce-from-end** _fn_: [ll-functionp] _x_: [listp]): [ll-anyp]
;; (**ll-reduce-with-init** _fn_: [ll-functionp] _init_: [ll-anyp] _x_: [listp]): [ll-anyp]
;;
;; Combines the elements of _x_ using an associative binary operation _fn_.
;;
;; `ll-reduce` is left-associative, `ll-reduce-from-end` is right-associative.
;; _init_ is logically placed before _x_ (or after it in case of
;; `ll-reduce-from-end`) and included in the reduction operation.
;;
;; If _x_ contains exactly one element, `ll-reduce` returns that element
;; without ever calling _fn_.
;; If _x_ is empty, `ll-reduce` calls _fn_ with no arguments to obtain the
;; return value.
;; If _x_ is empty, `ll-reduce-with-init` returns _init_ without ever calling
;; _fn_.
;;
;; `ll-reduce`, `ll-reduce-from-end`, `ll-reduce-with-init` passes parameters
;; to _fn_ in the same order as Common Lisp does,
;; i.e. `(fn accumulator current-item)`.
;;
;; The equivalent to missing `ll-reduce-from-end-with-init` is `ll-foldr`.
;;
;; For performance reasons, you should use `(apply fn x)` instead of
;; `(ll-reduce fn x)` when possible.
;;
;;     (ll-reduce '* '(1 2 3 4 5)) => 120
;;     (ll-reduce '- '(1 2 3 4))
;;       == (- (- (- 1 2) 3) 4)
;;       => -8
;;     (ll-reduce-from-end '- '(1 2 3 4))
;;       == (- 1 (- 2 (- 3 4)))
;;       => -2
;;     (ll-reduce '+ '()) => 0
;;     (ll-reduce '+ '(foo)) => FOO
;;     (ll-reduce 'list '(1 2 3 4)) => (((1 2) 3) 4)
;;     (ll-reduce-from-end 'list '(1 2 3 4)) => (1 (2 (3 4)))
;;     (ll-reduce-with-init 'list '() (1 2 3 4)) => ((((NIL 1) 2) 3) 4)
;;
;; See also `ll-foldl`, `ll-foldr`.
(if *call:enable-preeval*
    (defun ll-reduce-with-init (fn init x)
      (setq fn (eval fn))
      (while x
        (setq init (fn init (car x)) ; Common Lisp fn args order
              x (cdr x)))
      init)
    (defun ll-reduce-with-init (fn init x)
      (while x
        (setq init (apply fn (list init (car x))) ; Common Lisp fn args order
              x (cdr x)))
      init))
(defun ll-reduce (fn x)
  (if x
      (ll-reduce-with-init fn (car x) (cdr x))
      (apply fn x)))
(defun ll-reduce-from-end (fn x)
  (if (setq x (reverse x))
      (ll-foldl fn (car x) (cdr x))
      (apply fn x)))


;; ## List Searching

;; ### (**member** _v_: [ll-anyp] _x_: [listp]): [listp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### (**ll-member-if** _pred_: [ll-functionp] _x_: [listp]): [listp]
;; (**ll-member-if-not** _pred_: [ll-functionp] _x_: [listp]): [listp]
;;
;; TODO
;;
;; Identical to VisualLISP `vl-member-if` and `vl-member-if-not`
;; correspondingly.
(if (and (not *call:ignore-vlisp*)
         vl-member-if)
    ;; NOTE: BricsCAD LISP allows improper list, VisualLISP not.
    (setq ll-member-if vl-member-if)
    (if *call:enable-preeval*
        (defun ll-member-if (pred x)
          (setq pred (eval pred))
          (while (and x (not (pred (car x))))
            (setq x (cdr x)))
          x)
        (defun ll-member-if (pred x)
          (while (and x (not (apply pred (list (car x)))))
            (setq x (cdr x)))
          x)))
(if (and (not *call:ignore-vlisp*)
         vl-member-if-not)
    ;; NOTE: BricsCAD LISP allows improper list, VisualLISP not.
    (setq ll-member-if-not vl-member-if-not)
    (if *call:enable-preeval*
        (defun ll-member-if-not (pred x)
          (setq pred (eval pred))
          (while (and x (pred (car x)))
            (setq x (cdr x)))
          x)
        (defun ll-member-if-not (pred x)
          (while (and x (apply pred (list (car x))))
            (setq x (cdr x)))
          x)))

;; ### (**ll-position** _v_: [ll-anyp] _x_: [listp]) -> (or [ll-natnump] [null])
;;
;; TODO
;;
;; Identical to VisualLISP `vl-position`.
(if (and (not *call:ignore-vlisp*)
         vl-position)
    (setq ll-position vl-position)
    (defun ll-position (v x / n)
      (setq n 0)
      (while (and x (not (equal v (car x))))
        (setq n (1+ n)
              x (cdr x)))
      (if x n)))

;; ### (**ll-position-if** _pred_: [ll-functionp] _x_: [listp]) -> (or [ll-natnump] [null])
;; (**ll-position-if-not** _pred_: [ll-functionp] _x_: [listp]) -> (or [ll-natnump] [null])
;;
;; TODO
(if *call:enable-preeval*
  (progn
    (defun ll-position-if (pred x / n)
      (setq pred (eval pred)
            n 0)
      (while (and x
                  (not (pred (car x))))
        (setq n (1+ n)
              x (cdr x)))
      (if x n))
    (defun ll-position-if-not (pred x / n)
      (setq pred (eval pred)
            n 0)
      (while (and x
                  (pred (car x)))
        (setq n (1+ n)
              x (cdr x)))
      (if x n)))
  (progn
    (defun ll-position-if (pred x / n)
      (setq n 0)
      (while (and x
                  (not (apply pred (list (car x)))))
        (setq n (1+ n)
              x (cdr x)))
      (if x n))
    (defun ll-position-if-not (pred x / n)
      (setq n 0)
      (while (and x
                  (apply pred (list (car x))))
        (setq n (1+ n)
              x (cdr x)))
      (if x n))))

;; ### (**ll-inlistp** _v_: [ll-anyp] _x_: [listp]): [ll-booleanp]
;;
;; Returns `t` if item _v_ is present in the list _x_.
;;
;; Identical to BricsCAD LISP `vle-member`.
(cond
  ((and (not *call:ignore-lispex*)
        vle-member)
   (setq ll-inlistp vle-member))
  ((and (not *call:ignore-vlisp*)
        vl-position)
   (defun ll-inlistp (v x)
     (or (vl-position v x))))
  (t
   (defun ll-inlistp (v x)
     (or (member v x)))))

;; ### (**ll-mismatch** _x_: [listp] _y_: [listp]) -> (or [ll-natnump] [null])
;;
;; Compares _x_ and _y_. If they are the same length and the
;; corresponding elements match (according to `equal` function), it returns
;; `nil`. If there is a mismatch, the function returns the index (relative to
;; _x_) of the first mismatching element. This will be the leftmost pair of
;; elements that do not match, or the position at which the shorter of the two
;; otherwise-matching lists runs out.
(defun ll-mismatch (x y / n)
  (setq n 0)
  (while (and x
              y
              (equal (car x) (car y)))
    (setq x (cdr x)
          y (cdr y)
          n (1+ n)))
  (if (not (and (null x) (null y))) n))

;(if (and (not *call:ignore-lispex*) vle-search) ; FIXME: vl-search & ll-search should be different.
;    (setq ll-search vle-search)
;    (defun ll-search (x y) ;--- FIXME: (vle-search v x asidx)
;      nil)) ;--- TODO


;; ## List Modifying

;; ### (**subst** _new_: [ll-anyp] _old_: [ll-anyp] _x_: [listp]): [listp]
;;
;; Returns a list, with _new_ item replacing all occurrences of _old_ item.
;;
;; If _old_item is not found in _x_, returns _x_ unchanged.
;;
;; Unlike Common Lisp which performs operation on tree, searches top level only.
;; Use `ll-substree` for Common Lisp behaviour.
;;
;; AutoLISP built-in.

;; ### (**ll-subst-nth** _new_: [ll-anyp] _n_: [ll-natnump] _x_: [listp]): [listp]
;;
;; TODO
;;
;; Similar to BricsCAD LISP `vle-subst-nth`, but with different parameters order.
(if (and (not *call:ignore-lispex*)
         vle-subst-nth)
    (defun ll-subst-nth (new n x)
      (vle-subst-nth x n new))
    (defun ll-subst-nth (new n x)
      (setq x (ll-split-at n x))
      (if (cdr x)
          (append (car x) (cons new (cddr x)))
          (car x))))

;; ### (**ll-subst-first** _new_: [ll-anyp] _old_: [ll-anyp] _x_: [listp]): [listp]
;;
;; TODO
(if (and (not (or *call:ignore-lispex*
                  *call:ignore-vlisp*))
         vle-subst-nth
         vl-position)
    (defun ll-subst-first (new old x / n)
      (if (setq n (vl-position old x))
          (vle-subst-nth x n new)
          x))
    (defun ll-subst-first (new old x)
      (setq x (ll-split-at-first old x))
      (if (cdr x)
          (append (car x) (cons new (cdadr x)))
          (car x))))

;; ### (**ll-subst-last** _new_ _old_: [ll-anyp] _x_: [listp]): [listp]
;;
;; TODO
;;
;; Equivalent to `(reverse (ll-subst-first new old (reverse x)))`.
(defun ll-subst-last (new old x)
  (setq x (ll-split-at-last old x))
  (if (cdr x)
      (append (car x) (cons new (cdadr x)))
      (car x)))

;; ### (**ll-subst-if** _new_: [ll-anyp] _pred_: [ll-functionp] _x_: [listp]): [listp]
;; (**ll-subst-if-not** _new_: [ll-anyp] _pred_: [ll-functionp] _x_: [listp]): [listp]
;;
;; TODO
(if *call:enable-preeval*
    (progn
      (defun ll-subst-if (new pred x)
        (setq pred (eval pred))
        (mapcar (function (lambda (v) (if (pred v) new v)))
                x))
      (defun ll-subst-if-not (new pred x)
        (setq pred (eval pred))
        (mapcar (function (lambda (v) (if (pred v) v new)))
                x)))
    (progn
      (defun ll-subst-if (new pred x)
        (mapcar (function (lambda (v) (if (apply pred (list v)) new v)))
                x))
      (defun ll-subst-if-not (new pred x)
        (mapcar (function (lambda (v) (if (apply pred (list v)) v new)))
                x))))

;; ### (**ll-substree** _new_: [ll-anyp] _old_: [ll-anyp] _x_: [listp]): [listp]
;;
;; TODO
(defun ll-substree (new old x)
  ;; FIXME: OPTIMIZE: rewrite without recursion (use ll-maptree like ll-sublis?)
  (cond ((equal old x) new)
        ((atom x) x)
        (t (cons (ll-substree new old (car x))
                 (ll-substree new old (cdr x))))))

;; ### (**ll-sublis** _alist_: [listp] _x_: [listp]): [listp]
;;
;; Like [`ll-substree`](#ll-substree), except that it takes an association list
;; _alist_ of old-new pairs. Each element of _x_ is compared with the cars of
;; _alist_; if it matches, it is replaced by the corresponding cdr.
(defun ll-sublis (alist x)
  (ll-maptree
    (function (lambda (k / p)
                (if (setq p (assoc k alist)) (cdr p) k)))
    x))

;; ### (**ll-flatten** _x_: [listp]): [listp]
;;
;; TODO
(defun ll-flatten (x / y cur)
  ;; works for dotted lists
  ;; see ll-maptree
  (cond
    ((null x) nil)
    ((atom x) x)
    (t (while x
         (if (atom x) ; for processing '(1 2 . 3)
             (setq cur x
                   x nil)
             (setq cur (car x)
                   x (cdr x)))
         (while (not (atom cur)) ; (ll-consp cur)
           (if (cdr cur) (setq x (cons (cdr cur) x)))
           (setq cur (car cur)))
         ;; now cur is atom
         (setq y (cons cur y)))
       (reverse y))))

;; ### (**ll-insert** _new_: [ll-anyp] _n_: [ll-natnump] _x_: [listp]): [listp]
;;
;; TODO
(defun ll-insert (new n x)
  (setq x (ll-split-at n x))
  (append (car x) (cons new (cadr x))))

;; ### (**ll-remove** _v_: [ll-anyp] _x_: [listp]): [listp]
;;
;; TODO
;;
;; Identical to VisualLISP `vl-remove` and BricsCAD LISP `vle-remove-all`.
(if (and (not *call:ignore-vlisp*)
         vl-remove)
    (setq ll-remove vl-remove)
    (defun ll-remove (v x)
      (apply (function append)
             (subst nil (list v) (mapcar (function list) x)))))

;; ### (**ll-remove-if** _pred_: [ll-functionp] _x_: [listp]): [listp]
;; (**ll-remove-if-not** _pred_: [ll-functionp] _x_: [listp]): [listp]
;;
;; TODO
;;
;; Identical to VisualLISP `vl-remove-if` and `vl-remove-if-not`
;; correspondingly.
(if (and (not *call:ignore-vlisp*)
         vl-remove-if)
    (setq ll-remove-if vl-remove-if)
    (if *call:enable-preeval*
        (defun ll-remove-if (pred x)
          (setq pred (eval pred))
          (apply (function append)
                 (mapcar (function
                           (lambda (v)
                             (if (not (pred v))
                                 (list v))))
                         x)))
        (defun ll-remove-if (pred x)
          (apply (function append)
                 (mapcar (function
                           (lambda (v)
                             (if (not (apply pred (list v)))
                                 (list v))))
                         x)))))
(if (and (not *call:ignore-vlisp*)
         vl-remove-if-not)
    (setq ll-remove-if-not vl-remove-if-not)
    (if *call:enable-preeval*
        (defun ll-remove-if-not (pred x)
          (setq pred (eval pred))
          (apply (function append)
                 (mapcar (function
                           (lambda (v)
                             (if (pred v)
                                 (list v))))
                         x)))
        (defun ll-remove-if-not (pred x)
          (apply (function append)
                 (mapcar (function
                           (lambda (v)
                             (if (apply pred (list v))
                                 (list v))))
                         x)))))

;; ### (**ll-remove-nth** _n_: [ll-natnump] _x_: [listp]): [listp]
;;
;; TODO
;;
;; Identical to BricsCAD LISP `vle-remove-nth`.
(if (and (not *call:ignore-lispex*)
         vle-remove-nth)
    (setq ll-remove-nth vle-remove-nth)
    (defun ll-remove-nth (n x)
      (setq x (ll-split-at n x))
      (append (car x) (cdadr x))))

;; ### (**ll-remove-first** _v_: [ll-anyp] _x_: [listp]): [listp]
;;
;; TODO
;;
;; Identical to BricsCAD LISP `vle-remove-first`.
(if (and (not *call:ignore-lispex*)
         vle-remove-first)
    (setq ll-remove-first vle-remove-first)
    (defun ll-remove-first (v x)
      (setq x (ll-split-at-first v x))
      (append (car x) (cdadr x))))

;; ### (**ll-remove-last** _v_: [ll-anyp] _x_: [listp]): [listp]
;;
;; TODO
(defun ll-remove-last (v x)
  (setq x (ll-split-at-last v x))
  (append (car x) (cdadr x)))

;; ### (**ll-remove-duplicates** _x_: [listp]): [listp]
;;
;; TODO
(defun ll-remove-duplicates (x / y)
  (while x
    ;(or (member (car x) y)
    (or (ll-inlistp (car x) y)
        (setq y (cons (car x) y)))
    (setq x (cdr x)))
  (reverse y))

;; ### (**ll-remove-adjacent-duplicates** _x_: [listp]): [listp]
;;
;; TODO (faster then remove-duplicates on some (e.g. sorted) lists)
(defun ll-remove-adjacent-duplicates (x / y)
  (while x
    (or (equal (car x) (car y))
        (setq y (cons (car x) y)))
    (setq x (cdr x)))
  (reverse y))


;; ## List Tail Sharing

;; ### (**ll-tailp** _v_: [ll-anyp] _x_: [listp]): [ll-booleanp]
;;
;; Returns `t` if _v_ is the same as some tail of _x_, i.e., is `eq`
;; to _x_ or to any of its cdrs.
(defun ll-tailp (v x)
  (while (not (or (atom x) (eq v x)))
    (setq x (cdr x)))
  (eq v x))

;; ### (**ll-ldiff** _x_: [ll-anyp] _v_: [listp]): [listp]
;;
;; Returns a copy of a list up to a given cons cell.
;;
;; If _v_ is the same as some tail of _x_, i.e., is `eq` to one of the
;; cons cells of _x_, it returns a copy of the part of _x_ up to but not
;; including _v_; otherwise, a copy of _x_ is returned. For example,
;; `(ll-ldiff x (cddr x))` returns the first two elements of the list _x_.
;;
;; `(ll-ldiff x (ll-last-cdr x)) == (ll-butlast x)`
(defun ll-ldiff (x v / y)
  (while (not (or (atom x) (eq v x)))
    (setq y (cons (car x) y)
          x (cdr x)))
  (reverse y))


(if ll-features (ll-provide "call/lists"))

;; Expotrs
'(
  ;; Predictates:
  ;; atom
  ;; null
  ll-consp
  ;; listp
  ll-endp

  ;; Constructors:
  ;; cons
  ;; list
  ll-make-list
  ll-iota ll-range

  ;; Selectors:
  ;; car cdr
  ;; caar ... cddddr
  ll-first ll-second ll-third ll-fourth ll-fifth
  ll-sixth ll-seventh ll-eighth ll-ninth ll-tenth
  ll-rest
  ;; last
  ;; nth
  ll-nthcdr
  ll-firstn
  ll-butlast
  ll-butlastn
  ll-last-cons
  ll-lastn
  ll-sublist ll-subseq

  ;; Splitting:
  ll-split-if ll-split-if-not
  ll-split-at ll-split-at-first ll-split-at-last
  ll-split ll-split-all
  ll-partition ll-partition-all
  ll-separate

  ;; Operations:
  ;; length
  ll-list-length
  ;; reverse
  ;; append
  ll-revappend
  ll-copy-list ll-copy-tree
  ll-count
  ll-count-if ll-count-if-not

  ;; Iteration:
  ;; mapcar
  ll-maplist ll-maptree
  ll-some ll-every
  ll-foldl ll-foldr
  ll-reduce ll-reduce-from-end ll-reduce-with-init

  ;; Searching:
  ;; member
  ll-member-if ll-member-if-not
  ll-position
  ll-position-if ll-position-if-not
  ll-inlistp
  ll-mismatch

  ;; Modifying:
  ;; subst
  ll-subst-if ll-subst-if-not
  ll-subst-nth ll-subst-first ll-subst-last
  ll-substree ll-sublis
  ll-flatten
  ll-insert
  ll-remove
  ll-remove-if ll-remove-if-not
  ll-remove-nth ll-remove-first ll-remove-last
  ll-remove-duplicates ll-remove-adjacent-duplicates
  ;; sort sort-i

  ;; Tail Sharing:
  ll-tailp ll-ldiff
  )
