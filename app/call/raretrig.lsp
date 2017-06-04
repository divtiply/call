;;; call/raretrig.lsp --- Rarely Used Trigonometric and Hyperbolic Functions

;; Copyright (C) CALL Team and Contributors

;; This file is part of CALL (Common AutoLISP Library)
;; Released under the MIT license


;; # Rarely Used Trigonometric and Hyperbolic Functions
;; ====================================================

;; ### ll-cot
;; (**ll-cot** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the cotangent of _z_, were _z_ is in radians.
(defun ll-cot (z)
  ;; cot z = 1 / tan z = cos z / sin z
  (/ (cos z) (sin z))) ; (/ 1.0 (tan z))

;; ### ll-sec
;; (**ll-sec** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the secant of _z_, were _z_ is in radians.
(defun ll-sec (z)
  ;; sec z = 1 / cos z
  (/ 1.0 (cos z)))

;; ### ll-csc
;; > (**ll-csc** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the cosecant of _z_, were _z_ is in radians.
(defun ll-csc (z)
  ;; csc z = 1 / sin z
  (/ 1.0 (sin z)))

;; ### ll-acot
;; (**ll-acot** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the arccotangent in radians of _z_.
(defun ll-acot (z)
  ;; acot z = atan2(1, z)
  (atan 1.0 z))

;; ### ll-asec
;; (**ll-asec** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the arcsecant in radians of _z_.
(defun ll-asec (z)
  ;; asec z = acos 1/z
  (ll-acos (/ 1.0 z)))

;; ### ll-acsc
;; (**ll-acsc** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the arccosecant in radians of _z_.
(defun ll-acsc (z)
  ;; acsc z = asin 1/z
  (ll-asin (/ 1.0 z)))

;; ### ll-versin
;; (**ll-versin** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the versine (versed sine) of _z_.
(defun ll-versin (z)
  ;; versin z = 1 - cos z
  (- 1.0 (cos z)))

;; ### ll-vercos
;; (**ll-vercos** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the vercosine (versed cosine) of _z_.
(defun ll-vercos (z)
  ;; vercos z = 1 + cos z
  (+ 1.0 (cos z)))

;; ### ll-coversin
;; (**ll-coversin** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the coversine (coversed sine) of _z_.
(defun ll-coversin (z)
  ;; coversin z = 1 - sin z
  (- 1.0 (sin z)))

;; ### covercos
;;; > (**covercos** _z_) -> [ll-realp]
;;; > - _z_ : [numberp]
;;;
;;; Returns the covercosine (coversed cosine) of _z_.
(defun covercos (z)
  ;; covercos z = 1 + sin z
  (+ 1.0 (sin z)))

;; ### ll-haversin
;; (**ll-haversin** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the haversine (haversed sine) of _z_.
(defun ll-haversin (z)
  ;; haversin z = 1/2 versin z
  (/ (- 1.0 (cos z)) 2.0)) ; (/ (versin z) 2.0)

;; ### ll-havercos
;; (**ll-havercos** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the havercosine (haversed cosine) of _z_.
(defun ll-havercos (z)
  ;; havercos z = 1/2 vercos z
  (/ (+ 1.0 (cos z)) 2.0)) ; (/ (vercos z) 2.0)

;; ### ll-hacoversin
;; (**ll-hacoversin** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the hacoversine (hacoversed sine) of _z_.
(defun ll-hacoversin (z)
  ;; hacoversin z = 1/2 coversin z
  (/ (- 1.0 (sin z)) 2.0)) ; (/ (coversin z) 2.0)

;; ### ll-hacovercos
;; (**ll-hacovercos** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the hacovercosine (hacoversed cosine) of _z_.
(defun ll-hacovercos (z)
  ;; hacovercos z = 1/2 covercos z
  (/ (+ 1.0 (sin z)) 2.0)) ; (/ (covercos z) 2.0)

;; ### ll-exsec
;; > (**ll-exsec** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the exsecant of _z_.
(defun ll-exsec (z)
  ;; exsec z = sec z - 1
  (1- (/ 1.0 (cos z)))) ; (1- (sec z))

;; ### ll-excsc
;; (**ll-excsc** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the excosecant of _z_.
(defun ll-excsc (z)
  ;; excsc z = csc z - 1
  (1- (/ 1.0 (sin z)))) ; (1- (csc z))

;; ### ll-coth
;; (**ll-coth** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the hyperbolic cotangent of _z_.
(defun ll-coth (z)
  ;; coth z = (e^2z + 1) / (e^2z - 1)
  (setq z (exp (+ z z)))
  (/ (1+ z) (1- z)))

;; ### ll-sech
;; (**ll-sech** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the hyperbolic secant of _z_.
(defun ll-sech (z)
  ;; sech z = 2 / (e^z + e^-z)
  (setq z (exp z))
  (/ 2.0 (+ z (/ 1.0 z))))

;; ### ll-csch
;; (**ll-csch** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the hyperbolic cosecant of _z_.
(defun ll-csch (z)
  ;; csch z = 2 / (e^z - e^-z)
  (setq z (exp z))
  (/ 2.0 (- z (/ 1.0 z))))

;; ### ll-acoth
;; (**ll-acoth** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the hyperbolic area cotangent of _z_.
(defun ll-acoth (z)
  ;; acoth z = 1/2 ln((z + 1) / (z - 1)) ; z > 1 or z < -1 (|z| > 1)
  (/ (log (/ (+ z 1.0) (- z 1.0))) 2.0))

;; ### ll-asech
;; (**ll-asech** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the hyperbolic area secant of _z_.
(defun ll-asech (z)
  ;; asech z = ln(1/z + sqrt(1-z^2)/z)   ; 0 < z <= 1
  ;; asech z = ln(1/z + sqrt(1/z^2 - 1)) ; 0 < z <= 1
  ;; (log (/ (1+ (sqrt (- 1.0 (* z z)))) z))) ; TODO: <- test this, it seems best
  (log (+ (/ 1.0 z) (sqrt (1- (/ 1.0 (* z z)))))))

;; ### ll-acsch
;; (**ll-acsch** [numberp]: _z_) -> [ll-realp]
;;
;; Returns the hyperbolic area cosecant of _z_.
(defun ll-acsch (z)
  ;; acsch z = ln(1/z + sqrt(1-z^2) / z) ; z<>0
  ;; acsch z = ln(1/z + sqrt(1/z^2 + 1)) ; z<>0
  (log (+ (/ 1.0 z) (/ (sqrt (1+ (* z z))) (abs z))))) ; TODO: see asech


(if ll-features (ll-provide "call/raretrig"))

;; Exports
'(
  ll-cot ll-sec ll-csc
  ll-acot ll-asec ll-acsc
  ll-versin ll-vercos ll-coversin ll-covercos
  ll-haversin ll-havercos ll-hacoversin ll-hacovercos
  ll-exsec ll-excsc
  ll-coth ll-sech ll-csch
  ll-acoth ll-asech ll-acsch
)
