;;; call/list.lsp --- Lists

;; Copyright (C) CALL Team and Contributors

;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; # Lists
;; =======


;; ## List Predictates
;; -------------------

;; ### atom
;; (**atom** [ll-anyp]: _v_) -> [ll-booleanp]
;;
;; Returns `t` if _v_ is an *atom*, `nil` otherwise.
;;
;; All objects except *cons* cells are atoms. The symbol `nil` is an atom and
;; is also a list; it is the only Lisp object that is both.
;;
;; AutoLISP built-in.

;; ### null
;; (**null** [ll-anyp]: _v_) -> [ll-booleanp]
;;
;; Returns `t` if _v_ is `nil`, `nil` otherwise.
;;
;; This function is identical to `not`, but as a matter of clarity we use
;; `null` when object is considered a list and `not` when it is considered a
;; truth value.
;;
;; AutoLISP built-in.

;; ### ll-consp
;; (**ll-consp** [ll-anyp]: _v_) -> [ll-booleanp]
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

;; ### listp
;; (**listp** [ll-anyp]: _v_) -> [ll-booleanp]
;;
;; Returns `t` if _v_ is a *cons* cell or `nil`, `nil` otherwise.
;;
;;     (listp nil) => T
;;     (listp (cons 1 2)) => T
;;
;; AutoLISP built-in.

;; ### ll-endp
;; (**ll-endp** [listp]: _lst_) -> [ll-booleanp]
;;
;; Like `null`, but signals an error if _lst_ is not a *list*.
(defun ll-endp (lst)
  (car lst) ; raise error if not listp
  (null lst))


;; ## List Constructors
;; --------------------

;; ### cons
;; (**cons** [ll-anyp]: _a_ [ll-anyp]: _d_) -> [ll-consp]
;;
;; Returns a newly allocated *cons* cell whose first element is _a_ and second
;; element is _d_.
;;
;;     (cons 1 2) => (1 . 2)
;;     (cons 1 '()) => (1)
;;
;; AutoLISP built-in.

;; ### list
;; (**list** [ll-anyp]: _v_ ...) -> [listp]
;;
;; Returns a newly allocated list containing the _v_s as its elements.
;;
;;     (list 1 2 3 4) => (1 2 3 4)
;;     (list (list 1 2) (list 3 4)) => ((1 2) (3 4))
;;     (list) => NIL
;;
;; AutoLISP built-in.

;; ### ll-make-list
;; (**ll-make-list** [ll-natnump]: _n_ [ll-anyp]: _v_) -> [listp]
;;
;; Returns a list of _n_ elements, in wich each element is _v_.
;;
;;     (ll-make-list 7 'foo) => (FOO FOO FOO FOO FOO FOO FOO)
;;
(defun ll-make-list (n v / lst)
  (repeat n
    (setq lst (cons v lst))))

;; ### ll-iota
;; > (**ll-iota** [ll-natnump]: _n_) -> [listp]
;;
;; TODO
(defun ll-iota (n / lst)
  ;; same as (ll-iota-range 0 n 1)
  (repeat n
    (setq lst (cons (setq n (1- cnt)) lst))))

;; (defun ll-range (start end step / len lst)
;;   ;; FIXME: try (ll-range 0 10.1 0.5)
;;   (setq len (- end start)
;;         len (fix (if (zerop (rem len step))
;;                      (/ len step)
;;                      (1+ (/ len step)))
;;         end (+ start (* step len)))
;;   (repeat
;;     (setq lst (cons (setq end (- end step)) lst))))


;; ## List Selectors
;; -----------------

;; ### car
;; (**car** [ll-consp]: _p_) -> [ll-anyp]
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

;; ### cdr
;; (**cdr** [ll-consp]: _p_) -> [ll-anyp]
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

;; ### caar, cadr, cdar, cddr, ..., cddddr
;; (**cddddr** [ll-consp]: _p_) -> [ll-anyp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### ll-first {#ll-first}{#ll-second}{#ll-third}{#ll-fourth}{#ll-fifth}{#ll-sixth}{#ll-seventh}{#ll-eighth}{#ll-ninth}{#ll-tenth}
;; (**ll-first** [listp]: _lst_) -> [ll-anyp]
;;
;; Alias for `car`.
;;
;; Likewise, the functions `ll-second`, `ll-third`, `ll-fourth`, `ll-fifth`,
;; `ll-sixth`, `ll-seventh`, `ll-eighth`, `ll-ninth` and `ll-tenth` return the
;; given element of the _lst_.
;;
;; Identical to LispEx `vle-nth0` (`ll-first`) ... `vle-nth9` (`ll-tenth`).
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
    (setq
      ll-first   car
      ll-second  cadr
      ll-third   caddr
      ll-fourth  cadddr
      ll-fifth   (lambda (lst)       (car    (cddddr lst)))
      ll-sixth   (lambda (lst)       (cadr   (cddddr lst)))
      ll-seventh (lambda (lst)       (caddr  (cddddr lst)))
      ll-eighth  (lambda (lst)       (cadddr (cddddr lst)))
      ll-ninth   (lambda (lst) (car  (cddddr (cddddr lst))))
      ll-tenth   (lambda (lst) (cadr (cddddr (cddddr lst))))
      ))

;; ### ll-rest
;; (**ll-rest** [listp]: _lst_) -> [ll-anyp]
;;
;; Alias for `cdr`.
(setq ll-rest cdr)

;; ### last
;; (**last** [listp]: _lst_) -> [ll-anyp]
;;
;; Returns the last element of _lst_.
;;
;; If _lst_ is null, `nil` is returned.
;;
;;     (last '(a b c)) => C
;;
;; AutoLISP built-in.

;; ### nth
;; (**nth** [ll-natnump]: _n_ [listp]: _lst_) -> [ll-anyp]
;;
;; Returns the _n_-th element of _lst_.
;;
;; Elements are numbered starting with zero, so the car of list is element
;; number zero. If the length of _lst_ is _n_ or less, the value is `nil`.
;;
;;     (nth 2 '(1 2 3 4)) => 3
;;     (nth 10 '(1 2 3 4)) => NIL
;;
;; Equivalent to `(car (ll-nthcdr n list))`.
;; AutoLISP built-in.

;; ### ll-nthcdr
;; (**ll-nthcdr** [ll-natnump]: _n_ [listp]: _lst_) -> [ll-anyp]
;;
;; Returns the _n_-th cdr of _lst_. In other words, it skips past the first
;; _n_ links of list and returns what follows.
;;
;; If _n_ is zero, returns all of list. If the length of _lst_ is _n_ or less,
;; returns `nil`.
;;
;;     (ll-nthcdr 1 '(1 2 3 4)) => (2 3 4)
;;     (ll-nthcdr 9 '(1 2 3 4)) => NIL
;;
(if (and (not *call:ignore-lispex*)
         vle-sublist)
    (defun ll-nthcdr (n lst)
      (vle-sublist lst n 0))
    (defun ll-nthcdr (n lst)
      (nth n (list nil)) ; raise error if not natnump
      (while (and lst (< 0 n))
        (setq n (1- n)
              lst (cdr lst)))
      lst))

;; ### ll-firstn
;; (**ll-firstn** [listp]: _lst_ [ll-natnump]: _n_) -> [listp]
;;
;; Returns first _n_ elements of _lst_.
;;
;; If _lst_ is null, `nil` is returned.
(if (and (not *call:ignore-lispex*)
         vle-sublist)
    (defun ll-firstn (lst n)
      (if (/= 0 n)
          (vle-sublist lst 0 n)))
    (defun ll-firstn (lst n / out)
      (nth n (list nil)) ; raise error if not natnump
      (while (and lst (< 0 n))
        (setq n (1- n)
              out (cons (car lst) out)
              lst (cdr lst)))
      (reverse out)))

;; ### ll-butlast
;; (**ll-butlast** [listp]: _lst_) -> [listp]
;;
;; Returns a copy of _lst_ with the last element removed.
;;
;; Identical to LispEx `vle-remove-last`.
(if (and (not *call:ignore-lispex*)
         vle-remove-last)
    (setq ll-butlast vle-remove-last)
    (defun ll-butlast (lst)
      (reverse (cdr (reverse lst)))))

;; ### ll-butlastn
;; (**ll-butlastn** [listp]: _lst_ [ll-natnump]: _n_) -> [listp]
;;
;; Returns a copy of _lst_ with the last _n_ elements removed.
;;
;; FIXME: If _n_ is greater than zero it makes a copy of the list so as not to
;; damage the original list. In general, `(append (ll-butlastn lst n) (ll-lastn
;; lst n))` will return a list equal to _lst_.
(if (and (not *call:ignore-lispex*)
         vle-sublist)
    (defun ll-butlastn (lst n / len)
      (if (< n (setq len (length lst)))
          (vle-sublist lst 0 (- len n))))
    (defun ll-butlastn (lst n)
      (reverse (ll-nthcdr n (reverse lst)))))

;; ### ll-last-cons
;; (**ll-last-cons** [listp]: _lst_) -> [ll-consp]
;;
;; Returns the last cons of _lst_.
;;
;;     (ll-last-cons '(a b c)) => (C)
;;     (ll-last-cons (cons a b)) => B
(defun ll-last-cons (lst)
  ;; (while (ll-consp (cdr lst))
  (while (not (atom (cdr lst)))
    (setq lst (cdr lst)))
  lst)

;; ### ll-lastn
;; (**ll-lastn** [listp]: _lst_ [ll-natnump]: _n_) -> [listp]
;;
;; Returns the last _n_ conses (not the last _n_ elements) of _lst_.
;;
;; If _lst_ is null, `nil` is returned. If _n_ is non-`nil`, the
;; _n_-th-to-last element is returned instead, or the whole of list if _n_ is
;; bigger than list's length.
(if (and (not *call:ignore-lispex*)
         vle-sublist)
    (defun ll-lastn (lst n)
      (vle-sublist lst (- (length lst) n) 0))
    ;; Faster for larger Ns.
    (defun ll-lastn (lst n / out)
      (setq out lst
            lst (ll-nthcdr n lst))
      (while lst
        (setq out (cdr out)
              lst (cdr lst)))
      out))
    ;; Faster for smaller Ns.
    ;; (defun ll-lastn (lst n / out)
    ;;   ;; Non-conformity: returns a _copy_ of the list tail,
    ;;   ;; should return the tail itself.
    ;;   (nth n (list nil)) ; raise error if not natnump
    ;;   (setq lst (reverse lst))
    ;;   (while (and lst (< 0 n))
    ;;     (setq n (1- n)
    ;;           out (cons (car lst) out)
    ;;           lst (cdr lst)))
    ;;   out)

;; ### ll-sublist
;; (**ll-sublist** [listp]: _lst_ [ll-natnump]: _start_ [ll-natnump]: _len_) -> [listp]
;;
;; TODO: Returns the sublist of _lst_ starting with item at position _start_.
;; TODO: If _len_ <= 0 process to end of the list.
;;
;; FIXME: NOT EXACT ~~Equivalent to `(ll-firstn (ll-nthcdr start lst) len)`.~~
;;
;; Identical to LispEx `vle-sublist`.
(if (and (not *call:ignore-lispex*)
         vle-sublist)
    (setq ll-sublist vle-sublist)
    (defun ll-sublist (lst start len)
      (if (< 0 len)
          (ll-firstn (ll-nthcdr start lst) len)
          (ll-nthcdr start lst))))

;; ### ll-subseq
;; (**ll-subseq** [listp]: _lst_ [ll-natnump]: _start_ [ll-natnump]: _end_) -> [listp]
;;
;; TODO
(defun ll-subseq (lst start end)
  (ll-sublist lst start (if end (- end start) 0))) ; FIXME: 0 is bad for firstn (see solution above)


;; ## List Splitting
;; -----------------

;; ### ll-split-if {#ll-split-if}{#ll-split-if-not}
;; (**ll-split-if** [ll-functionp]: _pred_ [listp]: _lst_) -> [listp]
;; (**ll-split-if-not** _pred_ _lst_) -> [listp]
;;
;; TODO
(if *call:enable-preeval*
  (progn
    (defun ll-split-if (pred lst / out)
      (setq pred (eval pred))
      (while (and lst
                  (not (pred (car lst))))
        (setq out (cons (car lst) out)
              lst (cdr lst)))
      (list (reverse out) lst))
    (defun ll-split-if-not (pred lst / out)
      (setq pred (eval pred))
      (while (and lst
                  (pred (car lst)))
        (setq out (cons (car lst) out)
              lst (cdr lst)))
      (list (reverse out) lst)))
  (progn
    (defun ll-split-if (pred lst / out)
      (while (and lst
                  (not (apply pred (list (car lst)))))
        (setq out (cons (car lst) out)
              lst (cdr lst)))
      (list (reverse out) lst))
    (defun ll-split-if-not (pred lst / out)
      (while (and lst
                  (apply pred (list (car lst))))
        (setq out (cons (car lst) out)
              lst (cdr lst)))
      (list (reverse out) lst))))

;; ### ll-split-at
;; (**ll-split-at** [ll-natnump]: _n_ [listp]: _lst_) -> [listp]
;;
;; Returns a list of two sublists, where first sublist is first _n_ elements of
;; _lst_, and second sublist is the remaining elements.
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
;; Equivalent to `(list (ll-firstn list n) (ll-nthcdr n list))`, except that it
;; can be faster.
(if (and (not *call:ignore-lispex*)
         vle-sublist)
    (defun ll-split-at (n lst)
      (if (< 0 n)
          (list (vle-sublist lst 0 n) (vle-sublist lst n 0))
          (list nil lst)))
    (defun ll-split-at (n lst / out)
      (nth n (list nil)) ; raise error if not natnump
      (while (and lst (< 0 n))
        (setq n (1- n)
              out (cons (car lst) out)
              lst (cdr lst)))
      (list (reverse out) lst)))

;; ### ll-split-at-first
;; (**ll-split-at-first** [ll-anyp]: _v_ [listp]: _lst_) -> [listp]
;;
;; TODO
;;
;; Identical to LispEx `vle-list-split`.
;; Similar to Express Tools `acet-list-split`, but with reverse argument
;; order.
(if (and (not *call:ignore-lispex*)
         vle-list-split)
    (setq ll-split-at-first vle-list-split)
    (defun ll-split-at-first (v lst / out)
      (while (and lst
                  (not (equal v (car lst))))
        (setq out (cons (car lst) out)
              lst (cdr lst)))
      (list (reverse out) lst)))

;; ### ll-split-at-last
;; (**ll-split-at-last** [ll-anyp]: _v_ [listp]: _lst_) -> [listp]
;;
;; TODO
(defun ll-split-at-last (v lst / in out)
  ;; TODO: OPTIMIZE: search & save pointer, then search more
  (setq in (reverse lst))
  (while (and in
              (not (equal v (car in))))
    (setq out (cons (car in) out)
          in (cdr in)))
  (if in
      (cons (reverse (cdr in)) (cons (car in) out)) ;---!!! FIXME
      (list lst)))

;; ### ll-partition
;; (**ll-partition** [ll-natnump]: _n_ [listp]: _lst_) -> [listp]
;;
;; TODO
;;
;;     (ll-partition 2 '(a b c d e)) => ((a b)(c d))
;;
(defun ll-partition (n lst) ; split-every, chunk
  (*error* "ll-partition: Function not implemented")) ;--- TODO

;; ### ll-partition-all
;; (**ll-partition-all** [ll-natnump]: _n_ [listp]: _lst_) -> [listp]
;;
;; TODO
;;
;;     (ll-partition-all 2 '(a b c d e)) => ((a b)(c d)(e))
;;
(defun ll-partition-all (n lst)
  (*error* "ll-partition-all: Function not implemented")) ;--- TODO

;; ### ll-separate
;; (**ll-separate** [ll-functionp]: _pred_ [listp]: _lst_) -> [listp]
;;
;; TODO
;;
;;     (ll-separate 'numberp '(a b 1 c 2)) => ((1 2) (a b c))
;;
(if *call:enable-preeval*
    (defun ll-separate (pred lst / in out)
      (setq pred (eval pred))
      (foreach x lst
        (if (pred x)
            (setq in (cons x in))
            (setq out (cons x out))))
      (list (reverse in) (reverse out)))
    (defun ll-separate (pred lst / in out)
      (foreach x lst
        (if (apply pred (list x))
            (setq in (cons x in))
            (setq out (cons x out))))
      (list (reverse in) (reverse out))))


;; ## List Operations
;; ------------------

;; ### length
;; (**length** [listp]: _lst_) -> [natump]
;;
;; Returns the number of elements in _lst_.
;;
;; It is an error if _lst_ is an improper list.
;;
;;     (length '(1 2 3)) => 3
;;     (length '()) => 0
;;
;; AutoLISP built-in.

;; ### ll-list-length
;; (ll-list-length [listp]: _lst_) -> (or [ll-natnump] [null])
;;
;; Returns the length of _lst_, exactly like `length`, except that if _lst_
;; is an improper list, `nil` is returned.
;;
;; Identical to VisualLISP `vl-list-length`.
(if (and (not *call:ignore-vlisp*)
         vl-list-length)
    (setq ll-list-length vl-list-length)
    (defun ll-list-length (lst / len)
      (setq len 0)
      (while (and lst
                  (listp (setq lst (cdr lst))))
        (setq len (1+ len)))
      (if (null lst)
          len)))

;; ### reverse
;; (**reverse** [listp]: _lst_) -> [listp]
;;
;; Returns a list that has the same elements as _lst_, but in reverse order.
;;
;;     (reverse (list 1 2 3 4)) => (4 3 2 1)
;;
;; AutoLISP built-in.

;; ### append
;; (**append** [listp]: _lst_ ...) -> [listp]
;;
;; Returns a list that contains all of the elements of the given lists in
;; order.
;;
;;     (append '(x) '(y))        =>  (X Y)
;;     (append '(a) '(b c d))    =>  (A B C D)
;;     (append '(a (b)) '((c)))  =>  (A (B) (C))
;;
;; AutoLISP built-in.

;; ### ll-revappend
;; (**ll-revappend** [listp]: _lst_ [ll-anyp]: _v_) -> [listp]
;;
;; TODO
;; If v is a list then (ll-revappend lst v) == (append (reverse lst) v)
;;
;;     (ll-revappend '(1 2 3) '()) => (3 2 1)
;;     (ll-revappend '(1 2 3) '(a . b)) => (3 2 1 A . B)
;;     (ll-revappend '() '(a b c)) => (A B C)
;;     (ll-revappend '(1 2 3) 'a) => (3 2 1 . A)
;;     (ll-revappend '() 'a) =>  A ; degenerate case
;;
(defun ll-revappend (lst v)
  (while lst
    (setq v (cons (car lst) v)
          lst (cdr lst)))
  v)

;; FIXME: OPTIMIZE
;; http://www.gigamonkeys.com/book/beyond-lists-other-uses-for-cons-cells.html
(defun ll-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (ll-copy-list (cdr lst)))))

;; FIXME: OPTIMIZE
;; http://www.gigamonkeys.com/book/beyond-lists-other-uses-for-cons-cells.html
(defun ll-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (ll-copy-tree (car tr))
        (ll-copy-tree (cdr tr)))))

;; ### ll-count
;; (**ll-count** [ll-anyp]: _v_ [listp]: _lst_) -> [ll-natnump]
;;
;; Returns the number of elements of _lst_ which match _v_.
(defun ll-count (v lst / n)
  (setq n 0)
  (while (setq lst (member v lst))
    (setq lst (cdr lst)
          n (1+ n)))
  n)

;; ### ll-count-if {#ll-count-if}{#ll-count-if-not}
;; (**ll-count-if** [ll-functionp]: _pred_ [listp]: _lst_) -> [ll-natnump]
;; (**ll-count-if-not** [ll-functionp]: _pred_ [listp]: _lst_) -> [ll-natnump]
;;
;; TODO
(if *call:enable-preeval*
  (progn
    (defun ll-count-if (pred lst / n)
      ;; TODO: rewrite using while (see count)
      (setq pred (eval pred)
            n 0)
      (foreach x lst
        (if (pred x)
            (setq n (1+ n))))
      n)
    (defun ll-count-if-not (pred lst / n)
      ;; TODO: rewrite using while (see count)
      (setq pred (eval pred)
            n 0)
      (foreach x lst
        (or (pred x)
            (setq n (1+ n))))
      n))
  (progn
    (defun ll-count-if (pred lst / n)
      ;; TODO: rewrite using while (see count)
      (setq n 0)
      (foreach x lst
        (if (apply (function pred) (list x))
            (setq n (1+ n))))
      n))
    ;; TODO (defun count-if-not
    )


;; ## List Iteration
;; -----------------

;; ### mapcar
;; (**mapcar** [ll-functionp]: _fn_ [listp]: _lst_ ...) -> [listp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### ll-maplist
;; (**ll-maplist** [ll-functionp]: _fn_ [listp]: _lst_) -> [listp]
;;
;; TODO
(if *call:enable-preeval*
    (defun ll-maplist (fn lst / out) ; TODO: should it support improper lists?
      (car lst) ; raise error if not listp
      (setq fn (eval fn))
      (while lst
        (setq out (cons (fn lst) out)
              lst (cdr lst)))
      (reverse out))
    (defun ll-maplist (fn lst / out) ; TODO: should it support improper lists?
      (car lst) ; raise error if not listp
      (while lst
        (setq out (cons (apply fn (list lst)) out)
              lst (cdr lst)))
      (reverse out)))

;; ### ll-maptree
;; (**ll-maptree** [ll-functionp]: _fn_ [listp]: _tree_) -> [listp]
;;
;; TODO
; FIXME: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; FIXME: (if *call:enable-preeval*
(defun ll-maptree (fn tree / out pend cur pendlst newlst)
  ;; based on STDLIB std-mapatom
  ;; see ll-flatten
  (cond
    ((atom tree) (apply fn (list tree)))
    (t
     ;; loop until last element, for '(1 2 . 3) too
     (while (not (atom tree)) ; (consp tree)
       (setq pend (cons (car tree) pend)
             tree (cdr tree)))
     (setq out (if tree (apply fn (list tree)))
           pendlst nil
           newlst nil)
     (while pend
       (while pend
         (setq cur (car pend) pend (cdr pend))
         (if (atom cur)
             (setq out (cons (apply fn (list cur)) out))
             (progn
               (setq pendlst (cons pend pendlst)
                     newlst (cons out newlst) pend nil)
               (while (not (atom cur)) ; (consp cur)
                 (setq pend (cons (car cur) pend) cur (cdr cur)))
               (setq out (if cur (apply fn (list cur)))))))
       (while (and pendlst (null pend))
         (setq out (cons out (car newlst)) newlst (cdr newlst)
               pend (car pendlst) pendlst (cdr pendlst))))
     out)))

;; ### ll-some
;; (**ll-some** [ll-functionp]: _pred_ [listp]: _lst_) -> [ll-booleanp]
;;
;; TODO
;;
;; Similar to VisualLISP `vl-some`, but may accept only one list.
(if (and (not *call:ignore-vlisp*)
         vl-some)
    ;; NOTE: LispEx allows improper list, VisualLISP not.
    (defun ll-some (pred lst)
      (vl-some pred lst))
    (defun ll-some (pred lst)
      (and (ll-member-if pred lst))))

;; ### ll-every
;; (**ll-every** [ll-functionp]: _pred_ [listp]: _lst_) -> [ll-booleanp]
;;
;; TODO
;;
;; Similar to VisualLISP `vl-every`, but may accept only one list.
(if (and (not *call:ignore-vlisp*)
         vl-every)
    ;; NOTE: LispEx allows improper list, VisualLISP not.
    (defun ll-every (pred lst)
      (vl-every pred lst))
    (defun ll-every (pred lst)
      (not (ll-member-if-not pred lst))))

;; ### ll-foldl
;; (**ll-foldl** [ll-functionp]: _fn_ [ll-anyp]: _init_ [listp]: _lst_) -> [listp]
;;
;; The fundamental list iterator.
;; TODO
;; `ll-foldl` passes parameters to _fn_ in the same order as Scheme does,
;; i.e. `(fn current-item accumulator)`.
;;
;;     (ll-foldl cons '() '(1 2 3 4)) => (4 3 2 1)
;;     (ll-foldl list '() '(1 2 3 4)) => (4 (3 (2 (1 NIL))))
;;
;; See also `foldr`, `reduce`.
(if *call:enable-preeval*
    (defun ll-foldl (fn init lst)
      (setq fn (eval fn))
      (while lst
        (setq init (fn (car lst) init) ; Scheme fn args order
              lst (cdr lst)))
      init)
    (defun ll-foldl (fn init lst)
      (while lst
        (setq init (apply fn (list (car lst) init)) ; Scheme fn args order
              lst (cdr lst)))
      init))

;; ### ll-foldr
;; (**ll-foldr** [ll-functionp]: _fn_ [ll-anyp]: _init_ [listp]: _lst_) -> [listp]
;;
;; The fundamental list recursion iterator.
;; TODO
;;
;;     (ll-foldr 'cons '() '(1 2 3 4)) => (1 2 3 4)
;;     (ll-foldr 'list '() '(1 2 3 4)) => (1 (2 (3 (4 NIL)))
;;
;; See also `foldl`, `reduce`.
(defun ll-foldr (fn init lst)
  (ll-foldl fn init (reverse lst)))

;; ### ll-reduce {#ll-reduce}{#ll-reduce-from-end}{#ll-reduce-with-init}
;; (**ll-reduce** [ll-functionp]: _fn_ [listp]: _lst_) -> [ll-anyp]
;; (**ll-reduce-from-end** [ll-functionp]: _fn_ [listp]: _lst_) -> [ll-anyp]
;; (**ll-reduce-with-init** [ll-functionp]: _fn_ [ll-anyp]: _init_ [listp]: _lst_) -> [ll-anyp]
;;
;; Combines the elements of _lst_ using an associative binary operation _fn_.
;;
;; `ll-reduce` is left-associative, `ll-reduce-from-end` is right-associative.
;; _init_ is logically placed before _lst_ (or after it in case of
;; `ll-reduce-from-end`) and included in the reduction operation.
;;
;; If _lst_ contains exactly one element, `ll-reduce` returns that element
;; without ever calling _fn_.
;; If _lst_ is empty, `ll-reduce` calls _fn_ with no arguments to obtain the
;; return value.
;; If _lst_ is empty, `ll-reduce-with-init` returns _init_ without ever calling
;; _fn_.
;;
;; `ll-reduce`, `ll-reduce-from-end`, `ll-reduce-with-init` passes parameters
;; to _fn_ in the same order as Common Lisp does,
;; i.e. `(fn accumulator current-item)`.
;;
;; The equivalent to missing `ll-reduce-from-end-with-init` is `ll-foldr`.
;;
;; For performance reasons, you should use `(apply fn lst)` instead of
;; `(ll-reduce fn lst)` when possible.
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
    (defun ll-reduce-with-init (fn init lst)
      (setq fn (eval fn))
      (while lst
        (setq init (fn init (car lst)) ; Common Lisp fn args order
              lst (cdr lst)))
      init)
    (defun ll-reduce-with-init (fn init lst)
      (while lst
        (setq init (apply fn (list init (car lst))) ; Common Lisp fn args order
              lst (cdr lst)))
      init))
(defun ll-reduce (fn lst)
  (if lst
      (ll-reduce-with-init fn (car lst) (cdr lst))
      (apply fn lst)))
(defun ll-reduce-from-end (fn lst)
  (if (setq lst (reverse lst))
      (ll-foldl fn (car lst) (cdr lst))
      (apply fn lst)))


;; ## List Searching
;; -----------------

;; ### member
;; (**member** [ll-anyp]: _v_ [listp]: _lst_) -> [listp]
;;
;; TODO
;;
;; AutoLISP built-in.

;; ### ll-member-if {#ll-member-if}{#ll-member-if-not}
;; (**ll-member-if** [ll-functionp]: _pred_ [listp]: _lst_) -> [listp]
;; (**ll-member-if-not** [ll-functionp]: _pred_ [listp]: _lst_) -> [listp]
;;
;; TODO
;;
;; Identical to VisualLISP `vl-member-if` and `vl-member-if-not`
;; correspondingly.
(if (and (not *call:ignore-vlisp*)
         vl-member-if)
    ;; NOTE: LispEx allows improper list, VisualLISP not.
    (setq ll-member-if vl-member-if)
    (if *call:enable-preeval*
        (defun ll-member-if (pred lst)
          (setq pred (eval pred))
          (while (and lst (not (pred (car lst))))
            (setq lst (cdr lst)))
          lst)
        (defun ll-member-if (pred lst)
          (while (and lst (not (apply pred (list (car lst)))))
            (setq lst (cdr lst)))
          lst)))
(if (and (not *call:ignore-vlisp*)
         vl-member-if-not)
    ;; NOTE: LispEx allows improper list, VisualLISP not.
    (setq ll-member-if-not vl-member-if-not)
    (if *call:enable-preeval*
        (defun ll-member-if-not (pred lst)
          (setq pred (eval pred))
          (while (and lst (pred (car lst)))
            (setq lst (cdr lst)))
          lst)
        (defun ll-member-if-not (pred lst)
          (while (and lst (apply pred (list (car lst))))
            (setq lst (cdr lst)))
          lst)))

;; ### ll-position
;; (**ll-position** [ll-anyp]: _v_ [listp]: _lst_) -> (or [ll-natnump] [null])
;;
;; TODO
;;
;; Identical to VisualLISP `vl-position`.
(if (and (not *call:ignore-vlisp*)
         vl-position)
    (setq ll-position vl-position)
    (defun ll-position (v lst / n)
      (setq n 0)
      (while (and lst (not (equal v (car lst))))
        (setq n (1+ n)
              lst (cdr lst)))
      (if lst n)))

;; ### ll-position-if {#ll-position-if}{#ll-position-if-not}
;; (**ll-position-if** [ll-functionp]: _pred_ [listp]: _lst_) -> (or [ll-natnump] [null])
;; (**ll-position-if-not** [ll-functionp]: _pred_ [listp]: _lst_) -> (or [ll-natnump] [null])
;;
;; TODO
(if *call:enable-preeval*
  (progn
    (defun ll-position-if (pred lst / n)
      (setq pred (eval pred)
            n 0)
      (while (and lst
                  (not (pred (car lst))))
        (setq n (1+ n)
              lst (cdr lst)))
      (if lst n))
    (defun ll-position-if-not (pred lst / n)
      (setq pred (eval pred)
            n 0)
      (while (and lst
                  (pred (car lst)))
        (setq n (1+ n)
              lst (cdr lst)))
      (if lst n)))
  (progn
    (defun ll-position-if (pred lst / n)
      (setq n 0)
      (while (and lst
                  (not (apply pred (list (car lst)))))
        (setq n (1+ n)
              lst (cdr lst)))
      (if lst n))
    (defun ll-position-if-not (pred lst / n)
      (setq n 0)
      (while (and lst
                  (apply pred (list (car lst))))
        (setq n (1+ n)
              lst (cdr lst)))
      (if lst n))))

;; ### ll-inlistp
;; (**ll-inlistp** [ll-anyp]: _v_ [listp]: _lst_) -> [ll-booleanp]
;;
;; Returns `t` if item _v_ is present in the list _lst_.
;;
;; Identical to LispEx `vle-member`.
(cond
  ((and (not *call:ignore-lispex*)
        vle-member)
   (setq ll-inlistp vle-member))
  ((and (not *call:ignore-vlisp*)
        vl-position)
   (defun ll-inlistp (v lst)
     (or (vl-position v lst))))
  (t
   (defun ll-inlistp (v lst)
     (or (member v lst)))))

;; ### ll-mismatch
;; (**ll-mismatch** [listp]: _lst1_ [listp]: _lst2_) -> (or [ll-natnump] [null])
;;
;; Compares _lst1_ and _lst2_. If they are the same length and the
;; corresponding elements match (according to `equal` function), it returns
;; `nil`. If there is a mismatch, the function returns the index (relative to
;; _lst1_) of the first mismatching element. This will be the leftmost pair of
;; elements that do not match, or the position at which the shorter of the two
;; otherwise-matching lists runs out.
(defun ll-mismatch (lst1 lst2 / n)
  (setq n 0)
  (while (and lst1
              lst2
              (equal (car lst1) (car lst2)))
    (setq lst1 (cdr lst1)
          lst2 (cdr lst2)
          n (1+ n)))
  (if (not (and (null lst1) (null lst2))) n))

;(if (and (not *call:ignore-lispex*) vle-search) ; FIXME: vl-search & ll-search should be different.
;    (setq ll-search vle-search)
;    (defun ll-search (lst1 lst2) ;--- FIXME: (vle-search v lst asidx)
;      nil)) ;--- TODO


;; ## List Modifying
;; -----------------

;; ### subst
;; (**subst** [ll-anyp]: _new_ [ll-anyp]: _old_ [listp]: _lst_) -> [listp]
;;
;; Returns a list, with _new_ item replacing all occurrences of _old_ item.
;;
;; If _old_item is not found in _lst_, returns _lst_ unchanged.
;;
;; Unlike CommonLisp which performs operation on tree, searches top level only.
;; Use `ll-substree` for CommonLisp behaviour.
;;
;; AutoLISP built-in.

;; ### ll-subst-nth
;; (**ll-subst-nth** [ll-anyp]: _new_ [ll-natnump]: _n_ [listp]: _lst_) -> [listp]
;;
;; TODO
;;
;; Similar to LispEx `vle-subst-nth`, but with different parameters order.
(if (and (not *call:ignore-lispex*)
         vle-subst-nth)
    (defun ll-subst-nth (new n lst)
      (vle-subst-nth lst n new))
    (defun ll-subst-nth (new n lst)
      (setq lst (ll-split-at n lst))
      (if (cdr lst)
          (append (car lst) (cons new (cddr lst)))
          (car lst))))

;; ### ll-subst-first
;; (**ll-subst-first** [ll-anyp]: _new_ [ll-anyp]: _old_ [listp]: _lst_) -> [listp]
;;
;; TODO
(if (and (not (or *call:ignore-lispex*
                  *call:ignore-vlisp*))
         vle-subst-nth
         vl-position)
    (defun ll-subst-first (new old lst / n)
      (if (setq n (vl-position old lst))
          (vle-subst-nth lst n new)
          lst))
    (defun ll-subst-first (new old lst)
      (setq lst (ll-split-at-first old lst))
      (if (cdr lst)
          (append (car lst) (cons new (cdadr lst)))
          (car lst))))

;; ### ll-subst-last
;; (**ll-subst-last** _new_ [ll-anyp]: _old_ [listp]: _lst_) -> [listp]
;;
;; TODO
;;
;; Equivalent to `(reverse (subst-first new old (reverse lst)))`.
(defun ll-subst-last (new old lst)
  (setq lst (ll-split-at-last old lst))
  (if (cdr lst)
      (append (car lst) (cons new (cdadr lst)))
      (car lst)))

;; ### ll-subst-if {#ll-subst-if}{#ll-subst-if-not}
;; (**ll-subst-if** [ll-anyp]: _new_ [ll-functionp]: _pred_ [listp]: _lst_) -> [listp]
;; (**ll-subst-if-not** [ll-anyp]: _new_ [ll-functionp]: _pred_ [listp]: _lst_) -> [listp]
;;
;; TODO
(if *call:enable-preeval*
    (progn
      (defun ll-subst-if (new pred lst)
        (setq pred (eval pred))
        (mapcar (function (lambda (x) (if (pred x) new x)))
                lst))
      (defun ll-subst-if-not (new pred lst)
        (setq pred (eval pred))
        (mapcar (function (lambda (x) (if (pred x) x new)))
                lst)))
    (progn
      (defun ll-subst-if (new pred lst)
        (mapcar (function (lambda (x) (if (apply pred (list x)) new x)))
                lst))
      (defun ll-subst-if-not (new pred lst)
        (mapcar (function (lambda (x) (if (apply pred (list x)) x new)))
                lst))))

;; ### ll-substree
;; (**ll-substree** [ll-anyp]: _new_ [ll-anyp]: _old_ [listp]: _tree_) -> [listp]
;;
;; TODO
(defun ll-substree (new old tree)
  ;; FIXME: OPTIMIZE: rewrite without recursion (use ll-maptree like ll-sublis?)
  (cond ((equal old tree) new)
        ((atom tree) tree)
        (t (cons (ll-substree new old (car tree))
                 (ll-substree new old (cdr tree))))))

;; ### ll-sublis
;; (**ll-sublis** [listp]: _alist_ [listp]: _tree_) -> [listp]
;;
;; Like [`ll-substree`](#ll-substree), except that it takes an association list
;; _alist_ of old-new pairs. Each element of _tree_ is compared with the cars of
;; _alist_; if it matches, it is replaced by the corresponding cdr.
(defun ll-sublis (alist tree)
  (ll-maptree
    (function (lambda (k / p)
                (if (setq p (assoc k alist)) (cdr p) k)))
    tree))

;; ### ll-flatten
;; (**ll-flatten** [listp]: _lst_) -> [listp]
;;
;; TODO
(defun ll-flatten (lst / out cur)
  ;; works for dotted lists
  ;; see ll-maptree
  (cond
    ((null lst) nil)
    ((atom lst) lst)
    (t (while lst
         (if (atom lst) ; for processing '(1 2 . 3)
             (setq cur lst
                   lst nil)
             (setq cur (car lst)
                   lst (cdr lst)))
         (while (not (atom cur)) ; (ll-consp cur)
           (if (cdr cur) (setq lst (cons (cdr cur) lst)))
           (setq cur (car cur)))
         ;; now cur is atom
         (setq out (cons cur out)))
       (reverse out))))

;; ### ll-insert
;; (**ll-insert** [ll-anyp]: _new_ [ll-natnump]: _n_ [listp]: _lst_) -> [listp]
;;
;; TODO
(defun ll-insert (new n lst)
  (setq lst (ll-split-at n lst))
  (append (car lst) (cons new (cadr lst))))

;; ### ll-remove
;; (**ll-remove** [ll-anyp]: _v_ [listp]: _lst_) -> [listp]
;;
;; TODO
;;
;; Identical to VisualLISP `vl-remove` and LispEx `vle-remove-all`.
(if (and (not *call:ignore-vlisp*)
         vl-remove)
    (setq ll-remove vl-remove)
    (defun ll-remove (v lst)
      (apply (function append)
             (subst nil (list v) (mapcar (function list) lst)))))

;; ### ll-remove-if {ll-remove-if}{ll-remove-if-not}
;; (**ll-remove-if** [ll-functionp]: _pred_ [listp]: _lst_) -> [listp]
;; (**ll-remove-if-not** [ll-functionp]: _pred_ [listp]: _lst_) -> [listp]
;;
;; TODO
;;
;; Identical to VisualLISP `vl-remove-if` and `vl-remove-if-not`
;; correspondingly.
(if (and (not *call:ignore-vlisp*)
         vl-remove-if)
    (setq ll-remove-if vl-remove-if)
    (if *call:enable-preeval*
        (defun ll-remove-if (pred lst)
          (setq pred (eval pred))
          (apply (function append)
                 (mapcar (function
                           (lambda (x)
                             (if (not (pred x))
                                 (list x))))
                         lst)))
        (defun ll-remove-if (pred lst)
          (apply (function append)
                 (mapcar (function
                           (lambda (x)
                             (if (not (apply pred (list x)))
                                 (list x))))
                         lst)))))
(if (and (not *call:ignore-vlisp*)
         vl-remove-if-not)
    (setq ll-remove-if-not vl-remove-if-not)
    (if *call:enable-preeval*
        (defun ll-remove-if-not (pred lst)
          (setq pred (eval pred))
          (apply (function append)
                 (mapcar (function
                           (lambda (x)
                             (if (pred x)
                                 (list x))))
                         lst)))
        (defun ll-remove-if-not (pred lst)
          (apply (function append)
                 (mapcar (function
                           (lambda (x)
                             (if (apply pred (list x))
                                 (list x))))
                         lst)))))

;; ### ll-remove-nth
;; (**ll-remove-nth** [ll-natnump]: _n_ [listp]: _lst_) -> [listp]
;;
;; TODO
;;
;; Identical to LispEx `vle-remove-nth`.
(if (and (not *call:ignore-lispex*)
         vle-remove-nth)
    (setq ll-remove-nth vle-remove-nth)
    (defun ll-remove-nth (n lst)
      (setq lst (ll-split-at n lst))
      (append (car lst) (cdadr lst))))

;; ### ll-remove-first
;; (**ll-remove-first** [ll-anyp]: _v_ [listp]: _lst_) -> [listp]
;;
;; TODO
;;
;; Identical to LispEx `vle-remove-first`.
(if (and (not *call:ignore-lispex*)
         vle-remove-first)
    (setq ll-remove-first vle-remove-first)
    (defun ll-remove-first (v lst)
      (setq lst (ll-split-at-first v lst))
      (append (car lst) (cdadr lst))))

;; ### ll-remove-last
;; (**ll-remove-last** [ll-anyp]: _v_ [listp]: _lst_) -> [listp]
;;
;; TODO
(defun ll-remove-last (v lst)
  (setq lst (ll-split-at-last v lst))
  (append (car lst) (cdadr lst)))

;; ### ll-remove-duplicates
;; (**ll-remove-duplicates** [listp]: _lst_) -> [listp]
;;
;; TODO
(defun ll-remove-duplicates (lst / out)
  (while lst
    ;(or (member (car lst) out)
    (or (ll-inlistp (car lst) out)
        (setq out (cons (car lst) out)))
    (setq lst (cdr lst)))
  (reverse out))

;; ### ll-remove-adjacent-duplicates
;; (**ll-remove-adjacent-duplicates** [listp]: _lst_) -> [listp]
;;
;; TODO (faster then remove-duplicates on some (e.g. sorted) lists)
(defun ll-remove-adjacent-duplicates (lst / out)
  (while lst
    (or (equal (car lst) (car out))
        (setq out (cons (car lst) out)))
    (setq lst (cdr lst)))
  (reverse out))


;; ## List Tail Sharing
;; --------------------

;; ### ll-tailp
;; (**ll-tailp** [ll-anyp]: _v_ [listp]: _lst_) -> [ll-booleanp]
;;
;; Returns `t` if _v_ is the same as some tail of _lst_, i.e., is `eq`
;; to _lst_ or to any of its cdrs.
(defun ll-tailp (v lst)
  (while (not (or (atom lst) (eq v lst)))
    (setq lst (cdr lst)))
  (eq v lst))

;; ### ll-ldiff
;; (**ll-ldiff** [ll-anyp]: _lst_ [listp]: _v_) -> [listp]
;;
;; Returns a copy of a list up to a given cons cell.
;;
;; If _v_ is the same as some tail of _lst_, i.e., is `eq` to one of the
;; cons cells of _lst_, it returns a copy of the part of _lst_ up to but not
;; including _v_; otherwise, a copy of _lst_ is returned. For example,
;; `(ll-ldiff lst (cddr lst))` returns the first two elements of the list _lst_.
;;
;; `(ll-ldiff lst (ll-last-cdr lst)) == (ll-butlast lst)`
(defun ll-ldiff (lst v / out)
  (while (not (or (atom lst) (eq v lst)))
    (setq out (cons (car lst) out)
          lst (cdr lst)))
  (reverse out))


(if ll-features (ll-provide "call/list"))

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
