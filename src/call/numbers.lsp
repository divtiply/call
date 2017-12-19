;;; call/numbers.lsp --- Numbers

;; Copyright (C) CALL Team and Contributors

;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; # Numbers
;; =========


;; ## Predictates
;; --------------

;; ### ll-integerp
;; (**ll-integerp** _v_: [ll-anyp]): [ll-booleanp]
;;
;; Returns `t` if _v_ is an *integer*, `nil` otherwise.
;;
;; Identical to LispEx `vle-integerp`.
(if (and (not *call:ignore-lispex*)
         vle-integerp)
    (setq ll-integerp vle-integerp)
    (defun ll-integerp (v)
      (eq 'INT (type v))))

;; ### ll-realp
;; (**ll-realp** _v_: [ll-anyp]): [ll-booleanp]
;;
;; Returns `t` if _v_ is a *real*, `nil` otherwise.
;;
;; Identical to LispEx `vle-realp`.
(if (and (not *call:ignore-lispex*)
         vle-realp)
  (setq ll-realp vle-realp)
  (defun ll-realp (v)
    (eq 'REAL (type v))))

;; ### numberp
;; (**numberp** _v_: [ll-anyp]): [ll-booleanp]
;;
;; Returns `t` if _v_ is a *number* (either *integer* or *real*), `nil`
;; otherwise.
;;
;; Identical to LispEx `vle-numberp`.
;;
;; AutoLISP built-in.

;; ### ll-natnump
;; (**ll-natnump** _v_: [ll-anyp]): [ll-booleanp]
;;
;; Returns `t` if _v_ is a non-negative *integer*, `nil` otherwise.
;;
;; Equivalent to `(and (ll-integerp v) (not (ll-minusp v)))`.
(defun ll-natnump (v)
  (and (eq 'INT (type v))
       (not (minusp v))))

;; ### zerop
;; (**zerop** _z_: [numberp]): [ll-booleanp]
;;
;; Returns `t` if _z_ is zero, `nil` otherwise.
;;
;; AutoLISP built-in.

;; ### minusp
;; (**minusp** _z_: [numberp]): [ll-booleanp]
;;
;; Returns `t` if _z_ is less than zero, `nil` otherwise.
;;
;; AutoLISP built-in.

;; ### ll-plusp
;; (**ll-plusp** _z_: [numberp]): [ll-booleanp]
;;
;; Returns `t` if _z_ is greater than zero, `nil` otherwise.
(defun ll-plusp (z)
  (not (or (zerop z) (minusp z))))

;; ### ll-oddp
;; (**ll-oddp** _i_: [ll-integerp]): [ll-booleanp]
;;
;; Returns `t` if _i_ is odd, `nil` otherwise.
(defun ll-oddp (i)
  (not (zerop (logand i 1))))

;; ### ll-evenp
;; (**ll-evenp** _i_: [ll-integerp]): [ll-booleanp]
;;
;; Returns `t` if _i_ is even, `nil` otherwise.
(defun ll-evenp (i)
  (zerop (logand i 1)))


;; ## Conversions
;; --------------

;; ### float
;; (**float** _z_: [numberp]): [ll-realp]
;;
;; Converts _z_ to a *real*.
;;
;; AutoLISP built-in.

;; ### fix
;; (**fix** _z_: [numberp]): [ll-integerp]
;;
;; Converts _z_ to an *integer* by discarding fractional part.
;;
;; AutoLISP built-in.

;; TODO
;; itoa atoi atof


;; ## Arithmetic
;; -------------

;; ### + {#sum}
;; (**+** _z_: [numberp] ...): [numberp]
;;
;; Returns the sum of its arguments.
;;
;; AutoLISP built-in.

;; ### - {#difference}
;; (**-** _z_: [numberp] ...): [numberp]
;;
;; Returns its first argument minus the rest of its arguments.
;;
;; AutoLISP built-in.

;; ### * {#product}
;; (***** _z_: [numberp] ...): [numberp]
;;
;; Returns the product of its arguments.
;;
;; AutoLISP built-in.

;; ### / {#division}
;; (**/** _z_: [numberp] ...): [numberp]
;;
;; Returns its first argument divided by the rest of its arguments.
;;
;; AutoLISP built-in.

;; ### 1+ {#increment}
;; (**1+** _z_: [numberp]): [numberp]
;;
;; Increments _z_ by 1.
;;
;; Equivalent to `(+ z 1)`.
;;
;; AutoLISP built-in.

;; ### 1- {#decrement}
;; (**1-** _z_: [numberp]): [numberp]
;;
;; Decrements _z_ by 1.
;;
;; Equivalent to `(- z 1)`.
;;
;; AutoLISP built-in.

;; ### abs
;; (**abs** _z_: [numberp]): [numberp]
;;
;; Returns the absolute value of _z_.
;;
;; AutoLISP built-in.

;; ### min
;; (**min** _z_: [numberp] ...): [numberp]
;;
;; Returns the smallest of _z_s.
;;
;; AutoLISP built-in.

;; ### max
;; (**max** _z_: [numberp] ...): [numberp]
;;
;; Returns the largest of _z_s.
;;
;; AutoLISP built-in.

;; ### ll-clamp
;; (**ll-clamp _z_: [numberp] _lo_: [numberp] _hi_: [numberp]) => [numberp]
;;
;; Returns _z_ if it is larger than _lo_ and smaller than _hi_. In case _z_ is
;; smaller than _lo_, _lo_ is returned. If _z_ is larger than _hi_, _hi_
;; is returned.
(defun ll-clamp (z lo hi)
  (cond ((< z lo) lo)
        ((< hi z) hi)
        (t z)))

;; ### ll-lerp
;; (**ll-lerp _z_: [numberp] _w_: [numberp] _s_: [numberp]) => [numberp]
;;
;; Returns the linear interpolation between _z_ and _w_ by amount of _s_.
(defun ll-lerp (z w s)
  ;; Precise method, which guarantees z = w when s = 1.
  ;; https://en.wikipedia.org/wiki/Linear_interpolation#Programming_language_support
  (+ (* z (- 1 s)) (* w s)))
  ;; TODO: ? slerp nlerp
  ;; https://keithmaggio.wordpress.com/2011/02/15/math-magician-lerp-slerp-and-nlerp/

;; ### rem
;; (**rem** _z_: [numberp] _w_: [numberp] ...): [numberp]
;;
;; Returns the reminder from _z_ divided by _w_.
;;
;; See also [`ll-quot`](#ll-quot).
;;
;; AutoLISP built-in.

;; ### ll-mod
;; (**ll-mod** _z_: [numberp] _w_: [numberp]): [numberp]
;;
;; Returns the reminder from _z_ divided by _w_, with the same sign as _w_.
;;
;; Identical to BricsCAD LISP deprecated `mod`.
(if (and (not *call:ignore-lispex*)
         vle-extensions-active
         mod)
    (setq ll-mod mod)
    (defun ll-mod (z w)
      ;; http://mathforum.org/library/drmath/view/54377.html
      (rem (+ w (rem z w)) w)))

;; ### ll-quot
;; (**ll-quot** _z_: [numberp] _w_: [numberp]): [numberp]
;;
;; Returns the quotient from _z_ divided by _w_.
;;
;; The quotient is rounded towards zero, and the remainder will have the same
;; sign as _z_. In all cases quotient and remainder satisfy
;; _z_ = quotient * _w_ + reminder.
;;
;; See also [`rem`](#rem).
(defun ll-quot (z w)
  ;(fix (/ z w))) ; FIXME: see description
  (/ (- z (rem z w)) w)) ; FIXME: check

;; ### ll-frac
;; (**ll-frac** _z_: [numberp]): [ll-numberp]
;;
;; Returns the fractional part of _z_.
;;
;; Equivalent to `(- z (fix z))`.
(defun ll-frac (z)
  ;; if z is *integer* returns *integer* 0, otherwise returns *real*
  (rem z 1))

;; ### ll-signum
;; (**ll-signum** _z_: [numberp]): [ll-integerp]
;;
;; Returns the sign of _z_ as either -1, 0, or 1.
(defun ll-signum (z)
  (cond ((minusp z) -1)
        ((zerop z) 0)
        (t 1)))

;; ### ll-copysign
;; (**ll-copysign** _z_: [numberp] _w_: [numberp]): [numberp]
;;
;; Returns the value of _z_ with the sign of _w_.
(defun ll-copysign (z w)
  (if (minusp w)
      (- (abs z))
      (abs z)))

;; ### gcd
;; (**gcd** _z_: [numberp] _w_: [numberp]): [ll-integerp]
;;
;; Returns the greatest common divisor of its arguments.
;;
;; AutoLISP built-in.

;; ### ll-lcm
;; (**ll-lcm** _z_: [numberp] _w_: [numberp]): [ll-integerp]
;;
;; Returns the least common multiple of its arguments.
(defun ll-lcm (z w)
  (/ (* z w) (gcd z w)))

; ## Rounding
;; -----------

;; ### ll-truncate
;; (**ll-truncate** _z_: [numberp]): [ll-integerp]
;;
;; Returns _z_ converted to an integer by rounding towards zero.
;;
;;     (ll-truncate  1.2) =>  1
;;     (ll-truncate  1.7) =>  1
;;     (ll-truncate -1.2) => -1
;;     (ll-truncate -1.7) => -1
;;
;; Alias for [`fix`](#fix).
(setq ll-truncate fix)

;; ### ll-floor
;; (**ll-floor** _z_: [numberp]): [ll-integerp]
;;
;; Returns _z_ converted to an integer by rounding downward (towards negative
;; infinity).
;;
;;     (ll-floor  1.2) =>  1
;;     (ll-floor  1.7) =>  1
;;     (ll-floor -1.2) => -2
;;     (ll-floor -1.7) => -2
;;
;; Identical to LispEx `vle-floor`.
(if (and (not *call:ignore-lispex*)
         vle-floor
         (= (vle-floor -1) -1)) ; vle-floor was buggy before LispEx v6 (BricsCAD v14)
    (setq ll-floor vle-floor)
    (defun ll-floor (z)
      (if (minusp (rem z 1)) ; (if (minusp (ll-frac z))
          (1- (fix z))
          (fix z))))

;; ### ll-ceiling
;; (**ll-ceiling** _z_: [numberp]): [ll-integerp]
;;
;; Returns _z_ converted to an integer by rounding upward (towards positive
;; infinity).
;;
;;     (ll-ceiling  1.2) =>  2
;;     (ll-ceiling  1.7) =>  2
;;     (ll-ceiling -1.2) => -1
;;     (ll-ceiling -1.7) => -1
;;
;; Identical to LispEx `vle-ceiling`.
(if (and (not *call:ignore-lispex*)
         vle-ceiling
         (= (vle-ceiling 1) 1)) ; vle-ceiling was buggy before LispEx v6 (BricsCAD v14)
    (setq ll-ceiling vle-ceiling)
    (defun ll-ceiling (z)
      (if (> (rem z 1) 0) ; (if (ll-plusp (ll-frac z))
          (1+ (fix z))
          (fix z))))

;; ### ll-round, ll-round* {#ll-round}{#ll-round*}
;; (**ll-round** _z_: [numberp]): [ll-integerp]
;; (**ll-round*** _z_: [numberp]): [ll-integerp]
;;
;; Returns _z_, converted to an integer by rounding towards the nearest integer.
;; In the case of a tie (the argument is exactly halfway between two integers),
;; `ll-round` returns the nearest integer away from zero, while `ll-round*`
;; returns the even integer.
;;
;;     (ll-round  1.2) =>  1
;;     (ll-round  1.7) =>  2
;;     (ll-round -1.2) => -1
;;     (ll-round -1.7) => -2
;;     (ll-round  1.5) =>  2
;;     (ll-round  2.5) =>  2
;;
;; `ll-round` is equivalent to LispEx `vle-round`, `ll-round*` is ... (TODO).
(if (and (not *call:ignore-lispex*)
         vle-round)
    (setq ll-round vle-round)
    (defun ll-round (z)
      (fix (+ z (if (minusp z) -0.5 0.5)))))
(if (and (not *call:ignore-lispex*)
         vle-floor
         vle-round)
    (defun ll-round* (z)
      (cond ((zerop (rem z 0.5))
             (setq z (vle-floor z))
             (if (zerop (rem z 2))
                 z
                 (1+ z)))
            (t (vle-round z))))
    (defun ll-round* (z)
      (cond ((zerop (rem z 1)) (fix z))
            ((zerop (rem z 0.5))
             (setq z (fix z))
             (cond ((zerop (rem z 2)) z)
                   ((minusp z) (1- z))
                   (t (1+ z))))
            (t (ll-round z)))))


;; ## Exponential, Logarithmic and Root Extraction
;; -----------------------------------------------

;; ### expt
;; (**expt** _z_: [numberp] _w_: [numberp]): [numberp]
;;
;; Returns _z_ raised to the power of _w_.
;;
;; AutoLISP built-in.

;; ### exp
;; (**exp** _z_: [numberp]): [ll-realp]
;;
;; Returns Euler’s number E raised to the power of _z_.
;;
;; AutoLISP built-in.

;; ### log
;; (**log** _z_: [numberp]): [ll-realp]
;;
;; Returns the natural logarithm of _z_.
;;
;; AutoLISP built-in.

;; ### ll-log10
;; (**ll-log10** _z_: [numberp]): [ll-realp]
;;
;; Returns the base 10 logarithm of _z_.
;;
;; Identical to BricsCAD LISP `log10`.
(if (and (not *call:ignore-lispex*)
         vle-extensions-active
         log10)
    (setq ll-log10 log10)
    (defun ll-log10 (z)
      ;; log base z = log z / log base
      ;; log10 z = ln z / ln 10
      ;; log10 z = log z * 1/ln10 = log z * log10e
      (* (log z) 0.4342944819032518)))

;; ### ll-log2
;; (**ll-log2** _z_: [numberp]): [ll-realp]
;;
;; Returns the base 2 logarithm of _z_.
(defun ll-log2 (z)
  ;; log base z = log z / log base
  ;; log2 z = ln z / ln 2
  ;; log2 z = log z * 1/ln2 = log z * log2e
  (* (log z) 1.4426950408889634))

;; ### sqrt
;; (**sqrt** _z_: [numberp]): [ll-realp]
;;
;; Returns the square root of _z_.
;;
;; AutoLISP built-in.

;; ### ll-cbrt
;; (**ll-cbrt** _z_: [numberp]): [ll-realp]
;;
;; Returns the cube root of _z_.
(defun ll-cbrt (z)
  (expt z (/ 1.0 3.0)))


;; ## Trigonometric
;; ----------------

;; ### sin
;; (**sin** _z_: [numberp]): [ll-realp]
;;
;; Returns the sine of _z_, were _z_ is in radians.
;;
;; AutoLISP built-in.

;; ### cos
;; (**cos** _z_: [numberp]): [ll-realp]
;;
;; Returns the cosine of _z_, were _z_ is in radians.
;;
;; AutoLISP built-in.

;; ### ll-tan
;; (**ll-tan** _z_: [numberp]): [ll-realp]
;;
;; Returns the tangent of _z_, were _z_ is in radians.
;;
;; Identical to LispEx `vle-tan` or deprecated LispEx `tan`.
(if (and (not *call:ignore-lispex*)
         vle-tan)
    (setq ll-tan vle-tan)
    (defun ll-tan (z)
      ;; tan z = sin z / cos z
      (/ (sin z) (cos z))))

;; ### ll-asin
;; (**ll-asin** _z_: [numberp]): [ll-realp]
;;
;; Returns the arcsine in radians of _z_.
;;
;; It is an error if not -1 <= _z_ <= 1.
;;
;; Identical to BricsCAD LISP `asin`.
(if (and (not *call:ignore-lispex*)
         vle-extensions-active
         asin)
    (setq ll-asin asin)
    (defun ll-asin (z)
      ;; asin z = atan2(z, sqrt(1 - z^2))
      (atan z (sqrt (- 1.0 (* z z))))))

;; ### ll-acos
;; (**ll-acos** _z_: [numberp]): [ll-realp]
;;
;; Returns the arccosine in radians of _z_.
;;
;; It is an error if not -1 <= _z_ <= 1.
;;
;; Identical to BricsCAD LISP `acos`.
(if (and (not *call:ignore-lispex*)
         vle-extensions-active
         acos)
    (setq ll-acos acos)
    (defun ll-acos (z)
      ;; acos z = atan2(sqrt(1 - z^2), z)
      (atan (sqrt (- 1.0 (* z z))) z)))

;; ### atan
;; (**atan** _z_: [numberp]): [ll-realp]
;; (**atan** _x_: [numberp] _y_: [numberp]): [ll-realp]
;;
;; Returns the arctangent of _z_, in radians, if only one argument is supplied.
;; The two argument version returns the arctangent of _x_/_y_, in radians. If
;; _y_ is zero, returns an angle of +pi/2 or -pi/2 radians (+90 degrees or -90
;; degrees), depending on the sign of _x_.
;;
;; AutoLISP built-in.

;; ### ll-hypot
;; (**ll-hypot** _x_: [numberp] _y_: [numberp]): [ll-realp]
;;
;; Return the Euclidean norm, `sqrt(x*x + y*y)`.
(defun ll-hypot (x y)
  (sqrt (+ (* x x) (* y y))))


;; ## Angular Conversion
;; ---------------------

;; ### ll-dtr
;; (**ll-dtr** _z_: [numberp]): [ll-realp]
;;
;; Returns _z_ converted from degrees to radians.
(defun ll-dtr (z)
  ;; dtr z = pi/180 * z
  (* 0.017453292519943295 z))

;; ### ll-rtd
;; (**ll-rtd** _z_: [numberp]): [ll-realp]
;;
;; Returns _z_ converted from radians to degrees.
(defun ll-rtd (z)
  ;; rtd z = 180/pi * z
  (* 57.29577951308232 z))


;; ## Hyperbolic
;; -------------

;; ### ll-sinh
;; (**ll-sinh** _z_: [numberp]): [ll-realp]
;;
;; Returns the hyperbolic sine of _z_.
;;
;; Identical to BricsCAD LISP `sinh`.
(if (and (not *call:ignore-lispex*)
         vle-extensions-active
         sinh)
    (setq ll-sinh sinh)
    (defun ll-sinh (z)
      ;; sinh z = (e^z - e^-z) / 2
      (setq z (exp z))
      (/ (- z (/ 1.0 z)) 2.0)))

;; ### ll-cosh
;; (**ll-cosh** _z_: [numberp]): [ll-realp]
;;
;; Returns the hyperbolic cosine of _z_.
;;
;; Identical to BricsCAD LISP `cosh`.
(if (and (not *call:ignore-lispex*)
         vle-extensions-active
         cosh)
    (setq ll-cosh cosh)
    (defun ll-cosh (z)
      ;; cosh z = (e^z + e^-z) / 2
      (setq z (exp z))
      (/ (+ z (/ 1.0 z)) 2.0)))

;; ### ll-tanh
;; (**ll-tanh** _z_: [numberp]): [ll-realp]
;;
;; Returns the hyperbolic tangent of _z_.
;;
;; Identical to BricsCAD LISP `tanh`.
(if (and (not *call:ignore-lispex*)
         vle-extensions-active
         tanh)
    (setq ll-tanh tanh)
    (defun ll-tanh (z)
      ;; tanh z = (e^2z - 1) / (e^2z + 1)
      (setq z (exp (+ z z)))
      (/ (1- z) (1+ z))))

;; ### ll-asinh
;; (**ll-asinh** _z_: [numberp]): [ll-realp]
;;
;; Returns the hyperbolic area sine of _z_.
(defun ll-asinh (z)
  ;; asinh z = ln(z + sqrt(z^2 + 1))
  (log (+ z (sqrt (1+ (* z z))))))

;; ### ll-acosh
;; (**ll-acosh** _z_: [numberp]): [ll-realp]
;;
;; Returns the hyperbolic area cosine of _z_.
(defun ll-acosh (z)
  ;; acosh z = ln(z + sqrt(z^2 - 1)) ; z >= 1
  (log (+ z (sqrt (1- (* z z))))))

;; ### ll-atanh
;; (**ll-atanh** _z_: [numberp]): [ll-realp]
;;
;; Returns the hyperbolic area tangent of _z_.
;;
;; Identical to BricsCAD LISP `atanh`.
(if (and (not *call:ignore-lispex*)
         vle-extensions-active
         atanh)
    (setq ll-atanh atanh)
    (defun ll-atanh (z)
      ;; atanh z = 1/2 ln((1 + z) / (1 - z)) ; -1 < z < 1 (|z| < 1)
      (/ (log (/ (+ 1.0 z) (- 1.0 z))) 2.0)))


;; ## Constants
;; ------------

;; ### pi
;; [ll-realp]: **pi**
;;
;; Ratio of the circumference of a circle to its diameter, approx. 3.14159.
;;
;; AutoLISP built-in.

;; ### ll-e
;; [ll-realp]: **ll-e**
;;
;; Euler's or Napier’s constant, the base of natural logarithms, approx. 2.718.
(setq ll-e (exp 1)) ; 2.718281828459045

;; ### ll-ln10, ll-ln2
;; [ll-realp]: **ll-ln10**
;; [ll-realp]: **ll-ln2**
;;
;; Natural logarithm of 10 (approx. 2.303) or 2 (approx. 0.693), respectively.
(setq ll-ln10 (log 10) ; 2.302585092994046
      ll-ln2  (log 2)) ; 0.6931471805599453

;; ### ll-log10e, ll-log2e
;; [ll-realp]: **ll-log10e**
;; [ll-realp]: **ll-log2e**
;;
;; Base 10 and base 2 logarithm of E, approx. 0.434 and 1.443, respectively.
(setq ll-log10e (/ 1 (log 10)) ; 0.4342944819032518
      ll-log2e  (/ 1 (log 2))) ; 1.4426950408889634

;; ### ll-pi/2, ll-pi/4, ll-pi/180, ll-180/pi, ll-1/pi, ll-2pi
;;
;; Commonly-used trigonomethric constants.
(setq
  ll-pi/2   (/ pi 2)
  ll-pi/4   (/ pi 4)
  ll-pi/180 (/ pi 180) ; 0.017453292519943295
  ll-180/pi (/ 180 pi) ; 57.29577951308232
  ll-1/pi   (/ 1 pi)
  ll-2pi    (* 2.0 pi)) ; tau

;; ### ll-sqrt2, ll-sqrt1/2
;; [ll-realp]: **ll-sqrt2**
;; [ll-realp]: **ll-sqrt1/2**
;;
;; Square root of 2 (approx. 1.414); and square root of 1/2; equivalently, 1
;; over the square root of 2 (approx. 0.707), respectively.
(setq
  ll-sqrt2   (sqrt 2)    ; 1.4142135623730951
  ll-sqrt1/2 (sqrt 0.5)) ; 0.7071067811865476


(if ll-features (ll-provide "call/numbers"))

;; Exports
'(
  ;; Predictates:
  ll-integerp ll-realp
  ;; numberp
  ll-natnump
  ;; zerop minusp
  ll-plusp
  ll-oddp ll-evenp

  ;; Conversions:
  ;; float fix
  ;; itoa atoi atof

  ;; Arithmetic:
  ;; + - * / 1+ 1-
  ;; abs
  ;; min max
  ll-clamp ll-lerp
  ;; rem
  ll-mod ll-quot
  ll-frac
  ll-signum ll-copysign
  ;; gcd
  ll-lcm

  ;; Rounding:
  ll-truncate ll-floor ll-ceiling ll-round

  ;; Exponential, Logarithmic and Root Extraction:
  ;; expt
  ;; exp log
  ll-log10 ll-log2
  ;; sqrt
  ll-cbrt

  ;; Trigonometric:
  ;; sin cos
  ll-tan
  ll-asin ll-acos
  ;; atan
  ll-hypot

  ;; Angular Conversion:
  ll-rtd ll-dtr

  ;; Hyperbolic:
  ll-sinh ll-cosh ll-tanh
  ll-asinh ll-acosh ll-atanh

  ;; Constants:
  ;; pi
  ll-e
  ll-ln10 ll-ln2
  ll-log10e ll-log2e
  ll-pi/2 ll-pi/4 ll-pi/180 ll-180/pi ll-1/pi ll-2pi
  ll-sqrt2 ll-sqrt1/2

  ;; AutoLISP Specific Conversion Functions:
  ;; rtos distof LUNITS LUPREC
  ;; angtos angtof AUNTIS AUPREC
  ;; cvunit

  ;; AutoLISP Specific Geometric Functions:
  ;; angle distance
  ;; inters polar
  ;; osnap textbox
  ;; trans
  )
