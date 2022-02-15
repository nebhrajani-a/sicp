;; [[file:video_notes.org::*Primitive Elements][Primitive Elements:1]]
4
17.4
5
;; Primitive Elements:1 ends here

;; [[file:video_notes.org::*Means of Combination][Means of Combination:1]]
(+ 3 17.4 5)
;; Means of Combination:1 ends here

;; [[file:video_notes.org::*Means of Combination][Means of Combination:2]]
(+ 3 (* 5 6) 8 2)
;; Means of Combination:2 ends here

;; [[file:video_notes.org::*Means of Abstraction][Means of Abstraction:1]]
(define a (* 5 5))
a
(* a a)
(define b (+ a (* 5 a)))
b
(+ a (/ b 5))
;; Means of Abstraction:1 ends here

;; [[file:video_notes.org::square][square]]
(define (square x)
  (* x x))
;; square ends here

;; [[file:video_notes.org::*Means of Abstraction][Means of Abstraction:3]]
(define (square x)
  (* x x))
(square 10)
;; Means of Abstraction:3 ends here

;; [[file:video_notes.org::*Means of Abstraction][Means of Abstraction:4]]
(define square
  (lambda (x)
    (* x x)))

(square 25)
;; Means of Abstraction:4 ends here

;; [[file:video_notes.org::average][average]]
(define (average x y)
  (/ (+ x y) 2))
;; average ends here

;; [[file:video_notes.org::*Means of Abstraction][Means of Abstraction:6]]
(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))
(define (mean-square x y)
  (average (square x)
           (square y)))

(mean-square 2 3)
;; Means of Abstraction:6 ends here

;; [[file:video_notes.org::*Case Analysis in Lisp][Case Analysis in Lisp:1]]
(define (abs x)
  (cond ((< x 0) (- x))
        ((= x 0) 0)
        ((> x 0) x)))
(abs -3)
(abs 0)
(abs 5)
;; Case Analysis in Lisp:1 ends here

;; [[file:video_notes.org::abs][abs]]
(define (abs x)
  (if (< x 0)
      (- x)
      x))
;; abs ends here

;; [[file:video_notes.org::*Case Analysis in Lisp][Case Analysis in Lisp:3]]
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(abs -11)
(abs 0)
(abs 33)
;; Case Analysis in Lisp:3 ends here

;; [[file:video_notes.org::try][try]]
(define (try g x)
  (if (good-enough? g x)
      g
      (try (improve g x) x)))
;; try ends here

;; [[file:video_notes.org::sqrt][sqrt]]
(define (sqrt x)
  (try 1 x))
;; sqrt ends here

;; [[file:video_notes.org::improve][improve]]
(define (improve g x)
  (average g (/ x g)))
;; improve ends here

;; [[file:video_notes.org::good-enough?][good-enough?]]
(define (good-enough? g x)
  (< (abs (- (square g) x))
     0.00001))
;; good-enough? ends here

;; [[file:video_notes.org::*Finding Square Roots][Finding Square Roots:5]]
(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (improve g x)
  (average g (/ x g)))
(define (good-enough? g x)
  (< (abs (- (square g) x))
     0.00001))
(define (try g x)
  (if (good-enough? g x)
      g
      (try (improve g x) x)))
(define (sqrt x)
  (try 1 x))
(sqrt #i2)
(sqrt #i3)
(sqrt #i4)
;; Finding Square Roots:5 ends here

;; [[file:video_notes.org::*Finding Square Roots][Finding Square Roots:6]]
(define (sqrt x)
  (define (good-enough? g)
    (define (square g)
      (* g g))
    (define (abs y)
      (if (< y 0)
          (- y)
          y))
    (< (abs (- (square g) x))
       0.0001))
  (define (improve g)
    (define (average y z)
      (/ (+ y z) 2))
    (average g (/ x g)))
  (define (try g)
    (if (good-enough? g)
        g
        (try (improve g))))
  (try 1))

(sqrt #i2)
;; Finding Square Roots:6 ends here

;; [[file:video_notes.org::*Inbuilt/Primitive Procedures Aren't Special][Inbuilt/Primitive Procedures Aren't Special:1]]
(define (square x)
  (* x x))
square
+
;; Inbuilt/Primitive Procedures Aren't Special:1 ends here

;; [[file:video_notes.org::*Example][Example:1]]
(define (square x)
  (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)
;; Example:1 ends here

;; [[file:video_notes.org::*Example][Example:2]]
(sum-of-squares 3 4)
(+ (square 3) (square 4))
(+ (square 3) (* 4 4))
(+ (square 3) 16)
(+ (* 3 3) 16)
(+ 9 16)
25
;; Example:2 ends here

;; [[file:video_notes.org::peano-a][peano-a]]
(define (pa+ x y)
  (if (= x 0)
      y
      (pa+ (dec x) (inc y))))
;; peano-a ends here

;; [[file:video_notes.org::*Simple Peano Addition][Simple Peano Addition:2]]
(define (pa+ x y)
  (if (= x 0)
      y
      (pa+ (dec x) (inc y))))
(pa+ 3 4)
;; Simple Peano Addition:2 ends here

;; [[file:video_notes.org::*Simple Peano Addition][Simple Peano Addition:3]]
(pa+ 3 4)
(if (= 3 0)
    4
    (pa+ (dec 3) (inc 4)))
(pa+ 2 5)
...
(pa+ 1 6)
...
(pa+ 0 7)
7
;; Simple Peano Addition:3 ends here

;; [[file:video_notes.org::*Simple Peano Addition][Simple Peano Addition:4]]
(pa+ 3 4)
(pa+ 2 5)
(pa+ 1 6)
(pa+ 0 7)
7
;; Simple Peano Addition:4 ends here

;; [[file:video_notes.org::peano-b][peano-b]]
(define (pb+ x y)
  (if (= x 0)
      y
      (inc (pb+ (dec x) y))))
;; peano-b ends here

;; [[file:video_notes.org::*Another Peano Adder][Another Peano Adder:2]]
(pb+ 3 4)
(inc (pb+ 2 4))
(inc (inc (pb+ 1 4)))
(inc (inc (inc (pb+ 0 4))))
(inc (inc ((inc 4))))
(inc (inc 5))
(inc 6)
7
;; Another Peano Adder:2 ends here

;; [[file:video_notes.org::*Another Peano Adder][Another Peano Adder:3]]
(define (pb+ x y)
  (if (= x 0)
      y
      (inc (pb+ (dec x) y))))
(pb+ 3 4)
;; Another Peano Adder:3 ends here

;; [[file:video_notes.org::*Fibonacci Numbers][Fibonacci Numbers:1]]
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
(fib 10)
;; Fibonacci Numbers:1 ends here

;; [[file:video_notes.org::*Towers of Hanoi][Towers of Hanoi:1]]
(define (move n from to spare)
  (cond ((= n 1) (display "Move disk at rod ")
                 (display from)
                 (display " to rod ")
                 (display to)
                 (display ".\n"))
        (else
         (move (- n 1) from spare to)
         (move 1 from to spare)
         (move (- n 1) spare to from))))

(move 4 "A" "C" "B")
;; Towers of Hanoi:1 ends here

;; [[file:video_notes.org::iter-fib][iter-fib]]
(define (iter-fib n a b)
  (if (= n 1)
      b
      (iter-fib (dec n) b (+ a b))))

(define (fib n)
  (iter-fib n 0 1))
;; iter-fib ends here

;; [[file:video_notes.org::*Iterative Fibonacci][Iterative Fibonacci:2]]
(define (iter-fib n a b)
  (if (= n 1)
      b
      (iter-fib (dec n) b (+ a b))))

(define (fib n)
  (iter-fib n 0 1))
(fib 10)
;; Iterative Fibonacci:2 ends here

;; [[file:video_notes.org::*Abstracting Procedural Ideas][Abstracting Procedural Ideas:1]]
(define (sum-int a b)
  (if (> a b)
      0
      (+ a
         (sum-int (inc a) b))))

(sum-int 0 10)
;; Abstracting Procedural Ideas:1 ends here

;; [[file:video_notes.org::*Abstracting Procedural Ideas][Abstracting Procedural Ideas:2]]
(define (square x)
  (* x x))
(define (sum-sq a b)
  (if (> a b)
      0
      (+ (square a)
         (sum-sq (inc a) b))))

(sum-sq 0 4)
;; Abstracting Procedural Ideas:2 ends here

;; [[file:video_notes.org::*Abstracting Procedural Ideas][Abstracting Procedural Ideas:3]]
(define (sum-pi a b)
  (if (> a b)
      0
      (+ (/ 1
            (* a (+ a 2)))
         (sum-pi (+ a 4) b))))

(* 8 (sum-pi #i1 #i1000000))
;; Abstracting Procedural Ideas:3 ends here

;; [[file:video_notes.org::*Abstracting Procedural Ideas][Abstracting Procedural Ideas:4]]
(define (<name> a b)
  (if (> a b)
      0
      (+ (<term> a)
         (<name> (<next> a) b))))
;; Abstracting Procedural Ideas:4 ends here

;; [[file:video_notes.org::sum][sum]]
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
;; sum ends here

;; [[file:video_notes.org::*Abstracting Procedural Ideas][Abstracting Procedural Ideas:6]]
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (sum-int a b)
  (define (identity x) x)
  (sum identity
       a
       inc
       b))

(sum-int 0 10)
;; Abstracting Procedural Ideas:6 ends here

;; [[file:video_notes.org::*Abstracting Procedural Ideas][Abstracting Procedural Ideas:7]]
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (square x)
  (* x x))
(define (sum-sq a b)
  (sum square
       a
       inc
       b))

(sum-sq 0 4)
;; Abstracting Procedural Ideas:7 ends here

;; [[file:video_notes.org::*Abstracting Procedural Ideas][Abstracting Procedural Ideas:8]]
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (sum-pi a b)
  (sum (lambda (x)
         (/ 1
            (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(* 8 (sum-pi #i1 #i1000000))
;; Abstracting Procedural Ideas:8 ends here

;; [[file:video_notes.org::*Abstracting Procedural Ideas][Abstracting Procedural Ideas:9]]
(define (sum term a next b)
  (define (iter j ans)
    (if (> j b)
        ans
        (iter (next j)
              (+ (term j)
                 ans))))
  (iter a 0))

(define (sum-pi a b)
  (sum (lambda (x)
         (/ 1
            (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(* 8 (sum-pi #i1 #i1000000))
;; Abstracting Procedural Ideas:9 ends here

;; [[file:video_notes.org::no-dep-sqrt][no-dep-sqrt]]
(define (sqrt x)
  (define (good-enough? g)
    (define (square g)
      (* g g))
    (define (abs y)
      (if (< y 0)
          (- y)
          y))
    (< (abs (- (square g) x))
       0.0001))
  (define (improve g)
    (define (average y z)
      (/ (+ y z) 2))
    (average g (/ x g)))
  (define (try g)
    (if (good-enough? g)
        g
        (try (improve g))))
  (try 1))
;; no-dep-sqrt ends here

;; [[file:video_notes.org::*More on Square Roots][More on Square Roots:2]]
(define (sqrt x)
  (define (good-enough? g)
    (define (square g)
      (* g g))
    (define (abs y)
      (if (< y 0)
          (- y)
          y))
    (< (abs (- (square g) x))
       0.0001))
  (define (improve g)
    (define (average y z)
      (/ (+ y z) 2))
    (average g (/ x g)))
  (define (try g)
    (if (good-enough? g)
        g
        (try (improve g))))
  (try 1))
(sqrt #i2)
;; More on Square Roots:2 ends here

;; [[file:video_notes.org::fp-sqrt][fp-sqrt]]
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt x)
  (fixed-point
   (lambda (y) (average (/ x y)
                        y))
   1))
;; fp-sqrt ends here

;; [[file:video_notes.org::fixed-point][fixed-point]]
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (fixed-point f start)
  (define (close-enough-p x y)
    (< (abs (- x y))
       0.00001))
  (define (iter old new)
    (if (close-enough-p old new)
        new
        (iter new (f new))))
  (iter start (f start)))
;; fixed-point ends here

;; [[file:video_notes.org::*Fixed Points][Fixed Points:3]]
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (fixed-point f start)
  (define (close-enough-p x y)
    (< (abs (- x y))
       0.00001))
  (define (iter old new)
    (if (close-enough-p old new)
        new
        (iter new (f new))))
  (iter start (f start)))
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt x)
  (fixed-point
   (lambda (y) (average (/ x y)
                        y))
   1))
(sqrt #i2)
;; Fixed Points:3 ends here

;; [[file:video_notes.org::fp-avg-sqrt][fp-avg-sqrt]]
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (fixed-point f start)
  (define (close-enough-p x y)
    (< (abs (- x y))
       0.00001))
  (define (iter old new)
    (if (close-enough-p old new)
        new
        (iter new (f new))))
  (iter start (f start)))
(define (sqrt x)
  (fixed-point
   (avg-damp (lambda (y) (/ x y)))
   1))
;; fp-avg-sqrt ends here

;; [[file:video_notes.org::avg-damp][avg-damp]]
(define (average x y)
  (/ (+ x y) 2))
(define avg-damp
  (lambda (f)
    (lambda (x) (average (f x) x))))
;; avg-damp ends here

;; [[file:video_notes.org::*Damping Oscillations][Damping Oscillations:3]]
(define (average x y)
  (/ (+ x y) 2))
(define avg-damp
  (lambda (f)
    (lambda (x) (average (f x) x))))
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (fixed-point f start)
  (define (close-enough-p x y)
    (< (abs (- x y))
       0.00001))
  (define (iter old new)
    (if (close-enough-p old new)
        new
        (iter new (f new))))
  (iter start (f start)))
(define (sqrt x)
  (fixed-point
   (avg-damp (lambda (y) (/ x y)))
   1))
(sqrt #i2)
;; Damping Oscillations:3 ends here

;; [[file:video_notes.org::newton-sqrt][newton-sqrt]]
(define (square x)
  (* x x))
(define (sqrt x)
  (newton (lambda (y) (- x (square y)))
          1))
;; newton-sqrt ends here

;; [[file:video_notes.org::newton][newton]]
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (fixed-point f start)
  (define (close-enough-p x y)
    (< (abs (- x y))
       0.00001))
  (define (iter old new)
    (if (close-enough-p old new)
        new
        (iter new (f new))))
  (iter start (f start)))
(define (newton f guess)
  (define df (deriv f))
  (fixed-point
   (lambda (x) (- x
                  (/ (f x)
                     (df x))))
   guess))
;; newton ends here

;; [[file:video_notes.org::deriv][deriv]]
(define dx 0.0000001)

(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))
;; deriv ends here

;; [[file:video_notes.org::*Newton's Method][Newton's Method:4]]
(define dx 0.0000001)

(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))


((deriv (lambda (x) (* x x x))) 2)
;; Newton's Method:4 ends here

;; [[file:video_notes.org::*Newton's Method][Newton's Method:5]]
(define dx 0.0000001)

(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))


(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (fixed-point f start)
  (define (close-enough-p x y)
    (< (abs (- x y))
       0.00001))
  (define (iter old new)
    (if (close-enough-p old new)
        new
        (iter new (f new))))
  (iter start (f start)))
(define (newton f guess)
  (define df (deriv f))
  (fixed-point
   (lambda (x) (- x
                  (/ (f x)
                     (df x))))
   guess))
(define (square x)
  (* x x))
(define (sqrt x)
  (newton (lambda (y) (- x (square y)))
          1))
(sqrt #i2)
;; Newton's Method:5 ends here

;; [[file:video_notes.org::frac-proc][frac-proc]]
(define (+rat x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))
;; frac-proc ends here

;; [[file:video_notes.org::*Abstraction][Abstraction:2]]
(*rat (+rat x y)
      (+rat s t))
;; Abstraction:2 ends here

;; [[file:video_notes.org::*Data Object Creation][Data Object Creation:1]]
(define x (cons 1 2))
(car x)
(cdr x)
;; Data Object Creation:1 ends here

;; [[file:video_notes.org::make-rat][make-rat]]
(define (make-rat x y)
  (cons x y))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))
;; make-rat ends here

;; [[file:video_notes.org::*Data Object Creation][Data Object Creation:3]]
(define (+rat x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))
(define (make-rat x y)
  (cons x y))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define x (make-rat 1 2))
(define y (make-rat 1 4))
(define z (+rat x y))
(numer z)
(denom z)
;; Data Object Creation:3 ends here

;; [[file:video_notes.org::make-rat-gcd][make-rat-gcd]]
(define (make-rat x y)
  (let ((g (gcd x y)))
    (cons (/ x g)
          (/ y g))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))
;; make-rat-gcd ends here

;; [[file:video_notes.org::gcd][gcd]]
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;; gcd ends here

;; [[file:video_notes.org::*Data Object Creation][Data Object Creation:6]]
(define (make-rat x y)
  (let ((g (gcd x y)))
    (cons (/ x g)
          (/ y g))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))
(define (+rat x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

(define x (make-rat 1 2))
(define y (make-rat 1 4))
(define z (+rat x y))
(numer z)
(denom z)
;; Data Object Creation:6 ends here

;; [[file:video_notes.org::make-vec][make-vec]]
(define (make-vec x y)
  (cons x y))

(define (xcor v)
  (car v))

(define (ycor v)
  (cdr v))
;; make-vec ends here

;; [[file:video_notes.org::make-seg][make-seg]]
(define (make-seg v w)
  (cons v w))

(define (seg-start s)
  (car s))

(define (seg-end s)
  (cdr s))
;; make-seg ends here

;; [[file:video_notes.org::*Representing Points on a Plane][Representing Points on a Plane:3]]
(define (average x y)
  (/ (+ x y) 2))
(define (square x)
  (* x x))
(define (sqrt x)
  (define (good-enough? g)
    (define (square g)
      (* g g))
    (define (abs y)
      (if (< y 0)
          (- y)
          y))
    (< (abs (- (square g) x))
       0.0001))
  (define (improve g)
    (define (average y z)
      (/ (+ y z) 2))
    (average g (/ x g)))
  (define (try g)
    (if (good-enough? g)
        g
        (try (improve g))))
  (try 1))
(define (make-vec x y)
  (cons x y))

(define (xcor v)
  (car v))

(define (ycor v)
  (cdr v))
(define (make-seg v w)
  (cons v w))

(define (seg-start s)
  (car s))

(define (seg-end s)
  (cdr s))

(define (midpoint s)
  (let ((a (seg-start s))
        (b (seg-end s)))
    (make-vec
     (average (xcor a) (xcor b))
     (average (ycor a) (ycor b)))))

(define (length s)
  (let ((dx (- (xcor (seg-end s))
               (xcor (seg-start s))))
        (dy (- (ycor (seg-end s))
               (ycor (seg-start s)))))
    (sqrt (+ (square dx)
             (square dy)))))

(define side-a (make-vec #i3 #i0))
(define side-b (make-vec #i0 #i4))
(define segment (make-seg side-a side-b))

(length segment)

(define mp (midpoint segment))

(xcor mp)
(ycor mp)
;; Representing Points on a Plane:3 ends here

;; [[file:video_notes.org::*Representing Points on a Plane][Representing Points on a Plane:4]]
(define three-d-vec (cons 3 (cons 4 5)))
(car three-d-vec)
(car (cdr three-d-vec))
(cdr (cdr three-d-vec))
;; Representing Points on a Plane:4 ends here

;; [[file:video_notes.org::our-cons][our-cons]]
(define (our-cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))

(define (our-car x) (x 1))
(define (our-cdr x) (x 2))
;; our-cons ends here

;; [[file:video_notes.org::*Pairs][Pairs:2]]
(define (our-cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))

(define (our-car x) (x 1))
(define (our-cdr x) (x 2))
(define pair (our-cons 3 4))
(our-car pair)
(our-cdr pair)
;; Pairs:2 ends here

;; [[file:video_notes.org::*Pairs][Pairs:3]]
(lambda (pick)
  (cond ((= pick 1) 3)
        ((= pick 2) 4)))
;; Pairs:3 ends here

;; [[file:video_notes.org::*Pairs][Pairs:4]]
(define (our-cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))

(define (our-car x) (x 1))
(define (our-cdr x) (x 2))
(define three-d-vec (our-cons 3 (our-cons 4 5)))
(our-car three-d-vec)
(our-car (our-cdr three-d-vec))
(our-cdr (our-cdr three-d-vec))
(our-cdr three-d-vec)
;; Pairs:4 ends here

;; [[file:video_notes.org::*Pairs][Pairs:5]]
(lambda (pick)
  (cond ((= pick 1) 3)
        ((= pick 2) (lambda (pick)
                      (cond ((= pick 1) 4)
                            ((= pick 2) 5))))))
;; Pairs:5 ends here

;; [[file:video_notes.org::*Lecture 3A: Henderson Escher Example][Lecture 3A: Henderson Escher Example:1]]
(define (make-vec x y)
  (cons x y))

(define (xcor v)
  (car v))

(define (ycor v)
  (cdr v))
;; Lecture 3A: Henderson Escher Example:1 ends here

;; [[file:video_notes.org::vec-proc][vec-proc]]
(define (+vect v1 v2)
  (make-vec
   (+ (xcor v1) (xcor v2))
   (+ (ycor v1) (ycor v2))))

(define (scale v s)
  (make-vec
   (* s (xcor v))
   (* s (ycor v))))
;; vec-proc ends here

;; [[file:video_notes.org::*Lists][Lists:1]]
(cons (cons 1 2) (cons 3 4))
(cons (cons 1 (cons 2 3)) 4)
;; Lists:1 ends here

;; [[file:video_notes.org::*Lists][Lists:2]]
(cons 1 (cons 2 (cons 3 (cons 4 nil))))
;; Lists:2 ends here

;; [[file:video_notes.org::*Lists][Lists:3]]
(list 1 2 3 4)
;; Lists:3 ends here

;; [[file:video_notes.org::one-to-four][one-to-four]]
(define one-to-four (list 1 2 3 4))
;; one-to-four ends here

;; [[file:video_notes.org::*Lists][Lists:5]]
(define one-to-four (list 1 2 3 4))
(car one-to-four)
(cdr one-to-four)
(car (cdr one-to-four))
(cdr (cdr one-to-four))
(car (cdr (cdr (cdr one-to-four))))
(cdr (cdr (cdr (cdr one-to-four))))
;; Lists:5 ends here

;; [[file:video_notes.org::*Procedures on Lists][Procedures on Lists:1]]
(define one-to-four (list 1 2 3 4))
(define (scale-list l scale)
  (if (null? l)
      nil
      (cons (* scale (car l))
            (scale-list (cdr l) scale))))

(scale-list one-to-four 10)
;; Procedures on Lists:1 ends here

;; [[file:video_notes.org::map][map]]
(define (map p l)
  (if (null? l)
      nil
      (cons (p (car l))
            (map p (cdr l)))))
;; map ends here

;; [[file:video_notes.org::*Procedures on Lists][Procedures on Lists:3]]
(define (map p l)
  (if (null? l)
      nil
      (cons (p (car l))
            (map p (cdr l)))))
(define one-to-four (list 1 2 3 4))
(define (scale-list l s)
  (map (lambda (x) (* x s))
       l))

(scale-list one-to-four 20)
;; Procedures on Lists:3 ends here

;; [[file:video_notes.org::*Procedures on Lists][Procedures on Lists:4]]
(define (square x)
  (* x x))
(define (map p l)
  (if (null? l)
      nil
      (cons (p (car l))
            (map p (cdr l)))))
(define one-to-four (list 1 2 3 4))
(map square one-to-four)
;; Procedures on Lists:4 ends here

;; [[file:video_notes.org::for-each][for-each]]
(define (for-each proc l)
  (cond ((null? l) done)
        (else
         (proc (car l))
         (for-each proc (cdr l)))))
;; for-each ends here

;; [[file:video_notes.org::*Frames][Frames:1]]
(define (coord-map rect)
  (lambda (point)
    (+vect
     (+vect (scale (xcor point)
                   (horiz rect))
            (scale (ycor point)
                   (vert rect)))
     (origin rect))))
;; Frames:1 ends here

;; [[file:video_notes.org::*Pictures][Pictures:1]]
(define (make-picture seglist)
  (lambda (rect)
    (for-each
     (lambda (s)
       (drawline
        ((coord-map rect) (seg-start s))
        ((coord-map rect) (seg-end s))))
     seglist)))
;; Pictures:1 ends here

;; [[file:video_notes.org::*Pictures][Pictures:2]]
(define R (make-frame ;some vectors
           ))
(define draw-george-in-frame (make-picture ;some seglist
                              ))
(draw-george-in-frame R)
;; Pictures:2 ends here

;; [[file:video_notes.org::*Beside][Beside:1]]
(define (beside p1 p2 a)
  (lambda (rect)
    (p1 (make-frame
         (origin rect)
         (scale a (horiz rect))
         (vert rect)))
    (p2 (make-frame
         (+vect (origin rect)
                (scale a (horiz rect)))
         (scale (-1 a) (horiz rect))
         (vert rect)))))
;; Beside:1 ends here

;; [[file:video_notes.org::*Rotate-90][Rotate-90:1]]
(define (rot90 pict)
  (lambda (rect)
    (pict (make-frame
           (+vect (origin rect)
                  (horiz rect))
           (vert rect)
           (scale -1 (horiz rect))))))
;; Rotate-90:1 ends here

;; [[file:video_notes.org::*Means of Abstraction][Means of Abstraction:1]]
(define (right-push pict n a)
  (if (= n 0)
      pict
      (beside pict
              (right-push pict (dec n) a)
              a)))
;; Means of Abstraction:1 ends here

;; [[file:video_notes.org::*Means of Abstraction][Means of Abstraction:2]]
(define (push comb)
  (lambda (pict n a)
    ((repeated
      (lambda (p)
        (comb pict p a))
      n)
     pict)))

(define right-push (push beside))
;; Means of Abstraction:2 ends here

;; [[file:video_notes.org::sym-deriv][sym-deriv]]
(define (deriv expr var)
  (cond ((constant? expr var) 0)
        ((same-var? expr var) 1)
        ((sum? expr)
         (make-sum (deriv (a1 expr) var)
                   (deriv (a2 expr) var)))
        ((product? expr)
         (make-sum
          (make-product (m1 expr)
                        (deriv (m2 expr) var))
          (make-product (deriv (m1 expr) var)
                        (m2 expr))))))
;; sym-deriv ends here

;; [[file:video_notes.org::atom?][atom?]]
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
;; atom? ends here

;; [[file:video_notes.org::deriv-preds][deriv-preds]]
(define (constant? expr var)
  (and (atom? expr)
       (not (eq? expr var))))

(define (same-var? expr var)
  (and (atom? expr)
       (eq? expr var)))

(define (sum? expr)
  (and (not (atom? expr))
       (eq? (car expr) '+)))

(define (product? expr)
  (and (not (atom? expr))
       (eq? (car expr) '*)))
;; deriv-preds ends here

;; [[file:video_notes.org::deriv-makes][deriv-makes]]
(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))
;; deriv-makes ends here

;; [[file:video_notes.org::deriv-cadr][deriv-cadr]]
(define a1 cadr)
(define a2 caddr)

(define m1 cadr)
(define m2 caddr)
;; deriv-cadr ends here

;; [[file:video_notes.org::*Representation Implementation][Representation Implementation:5]]
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
(define (constant? expr var)
  (and (atom? expr)
       (not (eq? expr var))))

(define (same-var? expr var)
  (and (atom? expr)
       (eq? expr var)))

(define (sum? expr)
  (and (not (atom? expr))
       (eq? (car expr) '+)))

(define (product? expr)
  (and (not (atom? expr))
       (eq? (car expr) '*)))
(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))
(define a1 cadr)
(define a2 caddr)

(define m1 cadr)
(define m2 caddr)
(define (deriv expr var)
  (cond ((constant? expr var) 0)
        ((same-var? expr var) 1)
        ((sum? expr)
         (make-sum (deriv (a1 expr) var)
                   (deriv (a2 expr) var)))
        ((product? expr)
         (make-sum
          (make-product (m1 expr)
                        (deriv (m2 expr) var))
          (make-product (deriv (m1 expr) var)
                        (m2 expr))))))

(deriv '(+ (* a (* x x)) (+ (* b x) c)) 'x)
(deriv '(+ (* a (* x x)) (+ (* b x) c)) 'a)
(deriv '(+ (* a (* x x)) (+ (* b x) c)) 'b)
(deriv '(+ (* a (* x x)) (+ (* b x) c)) 'c)
;; Representation Implementation:5 ends here

;; [[file:video_notes.org::deriv-makes-2][deriv-makes-2]]
(define (make-sum a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        ((and (number? a1)
              (= a1 0))
         a2)
        ((and (number? a2)
              (= a2 0))
         a1)
        (else
         (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((and (number? m1)
              (number? m2))
         (* m1 m2))
        ((and (number? m1)
              (= m1 0))
         0)
        ((and (number? m2)
              (= m2 0))
         0)
        ((and (number? m1)
              (= m1 1))
         m2)
        ((and (number? m2)
              (= m2 1))
         m1)
        (else
         (list '+ m1 m2))))
;; deriv-makes-2 ends here

;; [[file:video_notes.org::*Simplification][Simplification:2]]
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
(define (constant? expr var)
  (and (atom? expr)
       (not (eq? expr var))))

(define (same-var? expr var)
  (and (atom? expr)
       (eq? expr var)))

(define (sum? expr)
  (and (not (atom? expr))
       (eq? (car expr) '+)))

(define (product? expr)
  (and (not (atom? expr))
       (eq? (car expr) '*)))
(define (make-sum a1 a2)
  (cond ((and (number? a1)
              (number? a2))
         (+ a1 a2))
        ((and (number? a1)
              (= a1 0))
         a2)
        ((and (number? a2)
              (= a2 0))
         a1)
        (else
         (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((and (number? m1)
              (number? m2))
         (* m1 m2))
        ((and (number? m1)
              (= m1 0))
         0)
        ((and (number? m2)
              (= m2 0))
         0)
        ((and (number? m1)
              (= m1 1))
         m2)
        ((and (number? m2)
              (= m2 1))
         m1)
        (else
         (list '+ m1 m2))))
(define a1 cadr)
(define a2 caddr)

(define m1 cadr)
(define m2 caddr)
(define (deriv expr var)
  (cond ((constant? expr var) 0)
        ((same-var? expr var) 1)
        ((sum? expr)
         (make-sum (deriv (a1 expr) var)
                   (deriv (a2 expr) var)))
        ((product? expr)
         (make-sum
          (make-product (m1 expr)
                        (deriv (m2 expr) var))
          (make-product (deriv (m1 expr) var)
                        (m2 expr))))))

(deriv '(+ (* a (* x x)) (+ (* b x) c)) 'x)
(deriv '(+ (* a (* x x)) (+ (* b x) c)) 'a)
(deriv '(+ (* a (* x x)) (+ (* b x) c)) 'b)
(deriv '(+ (* a (* x x)) (+ (* b x) c)) 'c)
;; Simplification:2 ends here

;; [[file:video_notes.org::rule-deriv-rules][rule-deriv-rules]]
(define deriv-rules
  '(
    ((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v u) (? v)) 0)

    ((dd (* (?c c) (? x)) (? v)) (* (: c) (dd (: x) (: v))))

    ((dd (+ (? x1) (? x2)) (? v))
     (+ (dd (: x1) (: v))
        (dd (: x2) (: v))))

    ((dd (* (? x1) (? x2)) (? v))
     (+ (* (: x1) (dd (: x2) (: v)))
        (* (: x2) (dd (: x1) (: v)))))
    ; ...
    ))
;; rule-deriv-rules ends here

;; [[file:video_notes.org::*Desired Behaviour][Desired Behaviour:1]]
(define dsimp
  (simplifier deriv-rules))

(dsimp '(dd (+ x y) x))
;; Desired Behaviour:1 ends here

;; [[file:video_notes.org::*Matcher][Matcher:1]]
(define (match pat expr dict)
  (cond ((eq? dict 'failed) 'failed)
        ; ... some other cases
        ((atom? expr) 'failed)
        (else
         (match (cdr pat)
           (cdr expr)
           (match (car pat)
             (car expr)
             dict)))))
;; Matcher:1 ends here

;; [[file:video_notes.org::rule-match][rule-match]]
(define (match pattern expression dict)
  (cond ((eq? dict 'failed) 'failed)

        ((atom? pattern)
         (if (atom? expression)
             (if (eq? pattern expression)
                 dict
                 'failed)
             'failed))

        ((arbitrary-constant? pattern)
         (if (constant? expression)
             (extend-dict pattern expression dict)
             'failed))

        ((arbitrary-variable? pattern)
         (if (variable? expression)
             (extend-dict pattern expression dict)
             'failed))

        ((arbitrary-expression? pattern)
         (extend-dict pattern expression dict))

        ((atom? expression) 'failed)

        (else
         (match (cdr pattern)
           (cdr expression)
           (match (car pattern)
             (car expression)
             dict)))))
;; rule-match ends here

;; [[file:video_notes.org::rule-instantiate][rule-instantiate]]
(define (instantiate skeleton dict)
  (cond ((atom? skeleton) skeleton)
        ((skeleton-evaluation? skeleton)
         (evaluate (evaluation-expression skeleton)
                   dict))
        (else (cons (instantiate (car skeleton) dict)
                    (instantiate (cdr skeleton) dict)))))
;; rule-instantiate ends here

;; [[file:video_notes.org::rule-evaluate][rule-evaluate]]
(define (evaluation-expression evaluation) (cadr evaluation))

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply (eval (lookup (car form) dict)
                   user-initial-environment)
             (map (lambda (v) (lookup v dict))
                  (cdr form)))))
;; rule-evaluate ends here

;; [[file:video_notes.org::rule-simplifier][rule-simplifier]]
(define (simplifier the-rules)
  (define (simplify-expression expression)
    (try-rules
     (if (compound? expression)
         (map simplify-expression expression)
         expression)))
  (define (try-rules expression)
    (define (scan rules)
      (if (null? rules)
          expression
          (let ((dict (match (pattern (car rules))
                        expression
                        (make-empty-dict))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-expression (instantiate
                                         (skeleton (car rules))
                                         dict))))))
    (scan the-rules))
  simplify-expression)
;; rule-simplifier ends here

;; [[file:video_notes.org::rule-dict][rule-dict]]
(define (make-empty-dict) '())

(define (extend-dict pat dat dictionary)
  (let ((vname (variable-name pat)))
    (let ((v (assq vname dictionary)))
      (cond ((not v)
             (cons (list vname dat) dictionary))
            ((eq? (cadr v) dat) dictionary)
            (else 'failed)))))

(define (lookup var dictionary)
  (let ((v (assq var dictionary)))
    (if (null? v)
        var
        (cadr v))))
;; rule-dict ends here

;; [[file:video_notes.org::rule-preds][rule-preds]]
(define (compound? exp) (pair? exp))
(define (constant? exp) (number? exp))
(define (variable? exp) (atom? exp))
(define (pattern rule) (car rule))
(define (skeleton rule) (cadr rule))

(define (arbitrary-constant? pat)
  (if (pair? pat) (eq? (car pat) '?c) false))

(define (arbitrary-expression? pat)
  (if (pair? pat) (eq? (car pat) '?) false))

(define (arbitrary-variable? pat)
  (if (pair? pat) (eq? (car pat) '?v) false))

(define (variable-name pat) (cadr pat))

(define (skeleton-evaluation? pat)
  (if (pair? pat) (eq? (car pat) ':) false))
;; rule-preds ends here

;; [[file:video_notes.org::rule-env-fix][rule-env-fix]]
(define user-initial-environment (scheme-report-environment 5))
(define (atom? x) (not (pair? x)))
;; rule-env-fix ends here

;; [[file:video_notes.org::*Usage][Usage:1]]
(define user-initial-environment (scheme-report-environment 5))
(define (atom? x) (not (pair? x)))
(define deriv-rules
  '(
    ((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v u) (? v)) 0)

    ((dd (* (?c c) (? x)) (? v)) (* (: c) (dd (: x) (: v))))

    ((dd (+ (? x1) (? x2)) (? v))
     (+ (dd (: x1) (: v))
        (dd (: x2) (: v))))

    ((dd (* (? x1) (? x2)) (? v))
     (+ (* (: x1) (dd (: x2) (: v)))
        (* (: x2) (dd (: x1) (: v)))))
    ; ...
    ))
(define (match pattern expression dict)
  (cond ((eq? dict 'failed) 'failed)

        ((atom? pattern)
         (if (atom? expression)
             (if (eq? pattern expression)
                 dict
                 'failed)
             'failed))

        ((arbitrary-constant? pattern)
         (if (constant? expression)
             (extend-dict pattern expression dict)
             'failed))

        ((arbitrary-variable? pattern)
         (if (variable? expression)
             (extend-dict pattern expression dict)
             'failed))

        ((arbitrary-expression? pattern)
         (extend-dict pattern expression dict))

        ((atom? expression) 'failed)

        (else
         (match (cdr pattern)
           (cdr expression)
           (match (car pattern)
             (car expression)
             dict)))))
(define (evaluation-expression evaluation) (cadr evaluation))

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply (eval (lookup (car form) dict)
                   user-initial-environment)
             (map (lambda (v) (lookup v dict))
                  (cdr form)))))
(define (instantiate skeleton dict)
  (cond ((atom? skeleton) skeleton)
        ((skeleton-evaluation? skeleton)
         (evaluate (evaluation-expression skeleton)
                   dict))
        (else (cons (instantiate (car skeleton) dict)
                    (instantiate (cdr skeleton) dict)))))
(define (simplifier the-rules)
  (define (simplify-expression expression)
    (try-rules
     (if (compound? expression)
         (map simplify-expression expression)
         expression)))
  (define (try-rules expression)
    (define (scan rules)
      (if (null? rules)
          expression
          (let ((dict (match (pattern (car rules))
                        expression
                        (make-empty-dict))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-expression (instantiate
                                         (skeleton (car rules))
                                         dict))))))
    (scan the-rules))
  simplify-expression)
(define (make-empty-dict) '())

(define (extend-dict pat dat dictionary)
  (let ((vname (variable-name pat)))
    (let ((v (assq vname dictionary)))
      (cond ((not v)
             (cons (list vname dat) dictionary))
            ((eq? (cadr v) dat) dictionary)
            (else 'failed)))))

(define (lookup var dictionary)
  (let ((v (assq var dictionary)))
    (if (null? v)
        var
        (cadr v))))
(define (compound? exp) (pair? exp))
(define (constant? exp) (number? exp))
(define (variable? exp) (atom? exp))
(define (pattern rule) (car rule))
(define (skeleton rule) (cadr rule))

(define (arbitrary-constant? pat)
  (if (pair? pat) (eq? (car pat) '?c) false))

(define (arbitrary-expression? pat)
  (if (pair? pat) (eq? (car pat) '?) false))

(define (arbitrary-variable? pat)
  (if (pair? pat) (eq? (car pat) '?v) false))

(define (variable-name pat) (cadr pat))

(define (skeleton-evaluation? pat)
  (if (pair? pat) (eq? (car pat) ':) false))
(define dsimp
  (simplifier deriv-rules))

(dsimp '(dd (* x x) x))
;; Usage:1 ends here

;; [[file:video_notes.org::rule-alg-rules][rule-alg-rules]]
(define algebra-rules
  '(
    ( ((? op) (?c c1) (?c c2))                (: (op c1 c2))                )
    ( ((? op) (?  e ) (?c c ))                ((: op) (: c) (: e))          )
    ( (+ 0 (? e))                             (: e)                         )
    ( (* 1 (? e))                             (: e)                         )
    ( (* 0 (? e))                             0                             )
    ( (* (?c c1) (* (?c c2) (? e )))          (* (: (* c1 c2)) (: e))       )
    ( (* (?  e1) (* (?c c ) (? e2)))          (* (: c ) (* (: e1) (: e2)))  )
    ( (* (* (? e1) (? e2)) (? e3))            (* (: e1) (* (: e2) (: e3)))  )
    ( (+ (?c c1) (+ (?c c2) (? e )))          (+ (: (+ c1 c2)) (: e))       )
    ( (+ (?  e1) (+ (?c c ) (? e2)))          (+ (: c ) (+ (: e1) (: e2)))  )
    ( (+ (+ (? e1) (? e2)) (? e3))            (+ (: e1) (+ (: e2) (: e3)))  )
    ( (+ (* (?c c1) (? e)) (* (?c c2) (? e))) (* (: (+ c1 c2)) (: e))       )
    ( (* (? e1) (+ (? e2) (? e3)))            (+ (* (: e1) (: e2)))         )
    ))
;; rule-alg-rules ends here

;; [[file:video_notes.org::*Algebraic Simplification][Algebraic Simplification:2]]
(define user-initial-environment (scheme-report-environment 5))
(define (atom? x) (not (pair? x)))
(define deriv-rules
  '(
    ((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v u) (? v)) 0)

    ((dd (* (?c c) (? x)) (? v)) (* (: c) (dd (: x) (: v))))

    ((dd (+ (? x1) (? x2)) (? v))
     (+ (dd (: x1) (: v))
        (dd (: x2) (: v))))

    ((dd (* (? x1) (? x2)) (? v))
     (+ (* (: x1) (dd (: x2) (: v)))
        (* (: x2) (dd (: x1) (: v)))))
    ; ...
    ))
(define algebra-rules
  '(
    ( ((? op) (?c c1) (?c c2))                (: (op c1 c2))                )
    ( ((? op) (?  e ) (?c c ))                ((: op) (: c) (: e))          )
    ( (+ 0 (? e))                             (: e)                         )
    ( (* 1 (? e))                             (: e)                         )
    ( (* 0 (? e))                             0                             )
    ( (* (?c c1) (* (?c c2) (? e )))          (* (: (* c1 c2)) (: e))       )
    ( (* (?  e1) (* (?c c ) (? e2)))          (* (: c ) (* (: e1) (: e2)))  )
    ( (* (* (? e1) (? e2)) (? e3))            (* (: e1) (* (: e2) (: e3)))  )
    ( (+ (?c c1) (+ (?c c2) (? e )))          (+ (: (+ c1 c2)) (: e))       )
    ( (+ (?  e1) (+ (?c c ) (? e2)))          (+ (: c ) (+ (: e1) (: e2)))  )
    ( (+ (+ (? e1) (? e2)) (? e3))            (+ (: e1) (+ (: e2) (: e3)))  )
    ( (+ (* (?c c1) (? e)) (* (?c c2) (? e))) (* (: (+ c1 c2)) (: e))       )
    ( (* (? e1) (+ (? e2) (? e3)))            (+ (* (: e1) (: e2)))         )
    ))
(define (match pattern expression dict)
  (cond ((eq? dict 'failed) 'failed)

        ((atom? pattern)
         (if (atom? expression)
             (if (eq? pattern expression)
                 dict
                 'failed)
             'failed))

        ((arbitrary-constant? pattern)
         (if (constant? expression)
             (extend-dict pattern expression dict)
             'failed))

        ((arbitrary-variable? pattern)
         (if (variable? expression)
             (extend-dict pattern expression dict)
             'failed))

        ((arbitrary-expression? pattern)
         (extend-dict pattern expression dict))

        ((atom? expression) 'failed)

        (else
         (match (cdr pattern)
           (cdr expression)
           (match (car pattern)
             (car expression)
             dict)))))
(define (evaluation-expression evaluation) (cadr evaluation))

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply (eval (lookup (car form) dict)
                   user-initial-environment)
             (map (lambda (v) (lookup v dict))
                  (cdr form)))))
(define (instantiate skeleton dict)
  (cond ((atom? skeleton) skeleton)
        ((skeleton-evaluation? skeleton)
         (evaluate (evaluation-expression skeleton)
                   dict))
        (else (cons (instantiate (car skeleton) dict)
                    (instantiate (cdr skeleton) dict)))))
(define (simplifier the-rules)
  (define (simplify-expression expression)
    (try-rules
     (if (compound? expression)
         (map simplify-expression expression)
         expression)))
  (define (try-rules expression)
    (define (scan rules)
      (if (null? rules)
          expression
          (let ((dict (match (pattern (car rules))
                        expression
                        (make-empty-dict))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-expression (instantiate
                                         (skeleton (car rules))
                                         dict))))))
    (scan the-rules))
  simplify-expression)
(define (make-empty-dict) '())

(define (extend-dict pat dat dictionary)
  (let ((vname (variable-name pat)))
    (let ((v (assq vname dictionary)))
      (cond ((not v)
             (cons (list vname dat) dictionary))
            ((eq? (cadr v) dat) dictionary)
            (else 'failed)))))

(define (lookup var dictionary)
  (let ((v (assq var dictionary)))
    (if (null? v)
        var
        (cadr v))))
(define (compound? exp) (pair? exp))
(define (constant? exp) (number? exp))
(define (variable? exp) (atom? exp))
(define (pattern rule) (car rule))
(define (skeleton rule) (cadr rule))

(define (arbitrary-constant? pat)
  (if (pair? pat) (eq? (car pat) '?c) false))

(define (arbitrary-expression? pat)
  (if (pair? pat) (eq? (car pat) '?) false))

(define (arbitrary-variable? pat)
  (if (pair? pat) (eq? (car pat) '?v) false))

(define (variable-name pat) (cadr pat))

(define (skeleton-evaluation? pat)
  (if (pair? pat) (eq? (car pat) ':) false))
(define dsimp
  (simplifier deriv-rules))

(define algsimp
  (simplifier algebra-rules))

(define (derivative x)
  (algsimp (dsimp x)))

(derivative '(dd (* x x) x))
(derivative '(dd (+ (+ x (* x 5)) (* x x)) x))
;; Algebraic Simplification:2 ends here

;; [[file:video_notes.org::complex-arith][complex-arith]]
(define (+c z1 z2)
  (make-rect
   (+ (real z1) (real z2))
   (+ (img z1) (img z2))))

(define (-c z1 z2)
  (make-rect
   (- (real z1) (real z2))
   (- (img z1) (img z2))))

(define (*c z1 z2)
  (make-pol
   (* (mag z1) (mag z2))
   (+ (ang z1) (ang z2))))

(define (/c z1 z2)
  (make-pol
   (/ (mag z1) (mag z2))
   (- (ang z1) (ang z2))))
;; complex-arith ends here

;; [[file:video_notes.org::*George's Representation][George's Representation:1]]
(define (make-rect x y)
  (cons x y))

(define (real z)
  (car z))

(define (img z)
  (cdr z))

(define (make-pol r a)
  (cons
   (* r (cos a))
   (* r (sin a))))

(define (mag z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (ang z)
  (atan (cdr z) (car z)))
;; George's Representation:1 ends here

;; [[file:video_notes.org::*Martha's Representation][Martha's Representation:1]]
(define (make-pol r a)
  (cons r a))

(define (mag z)
  (car z))

(define (ang z)
  (cdr z))

(define (real z)
  (* (car z)
     (cos (cdr z))))

(define (img z)
  (* (car z)
     (sin (cdr z))))

(define (make-rect x y)
  (cons
   (sqrt (square x) (square y))
   (atan y x)))
;; Martha's Representation:1 ends here

;; [[file:video_notes.org::type-arch][type-arch]]
(define (attach-type type contents)
  (cons type contents))

(define (type datum)
  (car datum))

(define (contents datum)
  (cdr datum))
;; type-arch ends here

;; [[file:video_notes.org::george-complex-procs][george-complex-procs]]
(define (make-rect x y)
  (attach-type 'rectangular (cons x y)))

(define (real-rectangular z)
  (car z))

(define (img-rectangular z)
  (cdr z))

(define (mag-rectangular z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (ang-rectangular z)
  (atan (cdr z) (car z)))
;; george-complex-procs ends here

;; [[file:video_notes.org::martha-complex-procs][martha-complex-procs]]
(define (make-pol r a)
  (attach-type 'polar (cons r a)))

(define (mag-polar z)
  (car z))

(define (ang-polar z)
  (cdr z))

(define (real-polar z)
  (* (car z)
     (cos (cdr z))))

(define (img-polar z)
  (* (car z)
     (sin (cdr z))))
;; martha-complex-procs ends here

;; [[file:video_notes.org::complex-manager][complex-manager]]
(define (real z)
  (cond ((rectangular? z)
         (real-rectangular (contents z)))
        ((polar? z)
         (real-polar (contents z)))))

(define (img z)
  (cond ((rectangular? z)
         (img-rectangular (contents z)))
        ((polar? z)
         (img-polar (contents z)))))

(define (mag z)
  (cond ((rectangular? z)
         (mag-rectangular (contents z)))
        ((polar? z)
         (mag-polar (contents z)))))

(define (ang z)
  (cond ((rectangular? z)
         (ang-rectangular (contents z)))
        ((polar? z)
         (ang-polar (contents z)))))
;; complex-manager ends here

;; [[file:video_notes.org::complex-manager-preds][complex-manager-preds]]
(define (rectangular? z)
  (eq? 'rectangular (type z)))

(define (polar? z)
  (eq? 'polar (type z)))
;; complex-manager-preds ends here

;; [[file:video_notes.org::*Usage][Usage:1]]
(define (square x)
  (* x x))
(define (attach-type type contents)
  (cons type contents))

(define (type datum)
  (car datum))

(define (contents datum)
  (cdr datum))
(define (make-rect x y)
  (attach-type 'rectangular (cons x y)))

(define (real-rectangular z)
  (car z))

(define (img-rectangular z)
  (cdr z))

(define (mag-rectangular z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (ang-rectangular z)
  (atan (cdr z) (car z)))
(define (make-pol r a)
  (attach-type 'polar (cons r a)))

(define (mag-polar z)
  (car z))

(define (ang-polar z)
  (cdr z))

(define (real-polar z)
  (* (car z)
     (cos (cdr z))))

(define (img-polar z)
  (* (car z)
     (sin (cdr z))))
(define (rectangular? z)
  (eq? 'rectangular (type z)))

(define (polar? z)
  (eq? 'polar (type z)))
(define (real z)
  (cond ((rectangular? z)
         (real-rectangular (contents z)))
        ((polar? z)
         (real-polar (contents z)))))

(define (img z)
  (cond ((rectangular? z)
         (img-rectangular (contents z)))
        ((polar? z)
         (img-polar (contents z)))))

(define (mag z)
  (cond ((rectangular? z)
         (mag-rectangular (contents z)))
        ((polar? z)
         (mag-polar (contents z)))))

(define (ang z)
  (cond ((rectangular? z)
         (ang-rectangular (contents z)))
        ((polar? z)
         (ang-polar (contents z)))))
(define (+c z1 z2)
  (make-rect
   (+ (real z1) (real z2))
   (+ (img z1) (img z2))))

(define (-c z1 z2)
  (make-rect
   (- (real z1) (real z2))
   (- (img z1) (img z2))))

(define (*c z1 z2)
  (make-pol
   (* (mag z1) (mag z2))
   (+ (ang z1) (ang z2))))

(define (/c z1 z2)
  (make-pol
   (/ (mag z1) (mag z2))
   (- (ang z1) (ang z2))))
(define a (make-rect 2 3))
(define b (make-pol 5 (tan (/ 3 4))))

(define result (+c a b))
(real result)
(img result)

(car a)
(car b)
(car result)
(cadr result)
(cddr result)
;; Usage:1 ends here

;; [[file:video_notes.org::complex-ddp-tbl][complex-ddp-tbl]]
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
;; complex-ddp-tbl ends here

;; [[file:video_notes.org::complex-george-puts][complex-george-puts]]
(put 'rectangular 'real real-rectangular)
(put 'rectangular 'img img-rectangular)
(put 'rectangular 'mag mag-rectangular)
(put 'rectangular 'ang ang-rectangular)
;; complex-george-puts ends here

;; [[file:video_notes.org::complex-martha-puts][complex-martha-puts]]
(put 'polar 'real real-polar)
(put 'polar 'img img-polar)
(put 'polar 'mag mag-polar)
(put 'polar 'ang ang-polar)
;; complex-martha-puts ends here

;; [[file:video_notes.org::complex-operate][complex-operate]]
(define (operate op object)
  (let ((proc (get (type object) op)))
    (if (not (null? proc))
        (proc (contents object))
        (error "Operation undefined."))))
;; complex-operate ends here

;; [[file:video_notes.org::complex-tbl-gen-ops][complex-tbl-gen-ops]]
(define (real obj)
  (operate 'real obj))

(define (img obj)
  (operate 'img obj))

(define (mag obj)
  (operate 'mag obj))

(define (ang obj)
  (operate 'ang obj))
;; complex-tbl-gen-ops ends here

;; [[file:video_notes.org::*Usage][Usage:1]]
(define (square x)
  (* x x))
(define (attach-type type contents)
  (cons type contents))

(define (type datum)
  (car datum))

(define (contents datum)
  (cdr datum))
(define (make-rect x y)
  (attach-type 'rectangular (cons x y)))

(define (real-rectangular z)
  (car z))

(define (img-rectangular z)
  (cdr z))

(define (mag-rectangular z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (ang-rectangular z)
  (atan (cdr z) (car z)))
(define (make-pol r a)
  (attach-type 'polar (cons r a)))

(define (mag-polar z)
  (car z))

(define (ang-polar z)
  (cdr z))

(define (real-polar z)
  (* (car z)
     (cos (cdr z))))

(define (img-polar z)
  (* (car z)
     (sin (cdr z))))
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(put 'rectangular 'real real-rectangular)
(put 'rectangular 'img img-rectangular)
(put 'rectangular 'mag mag-rectangular)
(put 'rectangular 'ang ang-rectangular)
(put 'polar 'real real-polar)
(put 'polar 'img img-polar)
(put 'polar 'mag mag-polar)
(put 'polar 'ang ang-polar)
(define (operate op object)
  (let ((proc (get (type object) op)))
    (if (not (null? proc))
        (proc (contents object))
        (error "Operation undefined."))))
(define (real obj)
  (operate 'real obj))

(define (img obj)
  (operate 'img obj))

(define (mag obj)
  (operate 'mag obj))

(define (ang obj)
  (operate 'ang obj))
(define (+c z1 z2)
  (make-rect
   (+ (real z1) (real z2))
   (+ (img z1) (img z2))))

(define (-c z1 z2)
  (make-rect
   (- (real z1) (real z2))
   (- (img z1) (img z2))))

(define (*c z1 z2)
  (make-pol
   (* (mag z1) (mag z2))
   (+ (ang z1) (ang z2))))

(define (/c z1 z2)
  (make-pol
   (/ (mag z1) (mag z2))
   (- (ang z1) (ang z2))))
(define a (make-rect 2 3))
(define b (make-pol 5 (tan (/ 3 4))))

(define result (+c a b))
(real result)
(img result)

(car a)
(car b)
(car result)
(cadr result)
(cddr result)
;; Usage:1 ends here

;; [[file:video_notes.org::*Usage][Usage:2]]
(real z)
(operate 'real z)
((get 'polar 'real) (contents z))
(real-polar (contents z))
;; Usage:2 ends here

;; [[file:video_notes.org::asn-rat-sel][asn-rat-sel]]
(define (numer x) (car x))
(define (denom x) (cdr x))
;; asn-rat-sel ends here

;; [[file:video_notes.org::asn-rat][asn-rat]]
(define (make-rat n d)
  (attach-type 'rational (cons n d)))

(put 'rational 'add +rat)
(put 'rational 'mul *rat)
;; asn-rat ends here

;; [[file:video_notes.org::*Implementation][Implementation:3]]
(define (+rat x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))
;; Implementation:3 ends here

;; [[file:video_notes.org::asn-complex][asn-complex]]
(define (make-complex z)
  (attach-type 'complex z))

(define (+complex z1 z2)
  (make-complex (+c z1 z2)))

(define (-complex z1 z2)
  (make-complex (-c z1 z2)))

(define (*complex z1 z2)
  (make-complex (*c z1 z2)))

(define (/complex z1 z2)
  (make-complex (/c z1 z2)))

(put 'complex 'add +complex)
(put 'complex 'sub -complex)
(put 'complex 'mul *complex)
(put 'complex 'div /complex)
;; asn-complex ends here

;; [[file:video_notes.org::asn-num][asn-num]]
(define (make-number n)
  (attach-type 'number n))

(define (+number x y)
  (make-number (+ x y)))

(define (-number x y)
  (make-number (- x y)))

(define (*number x y)
  (make-number (* x y)))

(define (/number x y)
  (make-number (/ x y)))

(put 'number 'add +number)
(put 'number 'sub -number)
(put 'number 'mul *number)
(put 'number 'div /number)
;; asn-num ends here

;; [[file:video_notes.org::asn-toplevel][asn-toplevel]]
(define (add x y)
  (operate-2 'add x y))

(define (sub x y)
  (operate-2 'sub x y))

(define (mul x y)
  (operate-2 'mul x y))

(define (div x y)
  (operate-2 'div x y))
;; asn-toplevel ends here

;; [[file:video_notes.org::asn-operate-2][asn-operate-2]]
(define (operate-2 op arg1 arg2)
  (if (eq? (type arg1) (type arg2))
      (let ((proc (get (type arg1) op)))
        (if (not (null? proc))
            (proc (contents arg1)
                  (contents arg2))
            (error "Operation undefined.")))
      (error "Argument type mismatch.")))
;; asn-operate-2 ends here

;; [[file:video_notes.org::*Usage][Usage:1]]
(define (square x)
  (* x x))
(define (attach-type type contents)
  (cons type contents))

(define (type datum)
  (car datum))

(define (contents datum)
  (cdr datum))
(define (make-rect x y)
  (attach-type 'rectangular (cons x y)))

(define (real-rectangular z)
  (car z))

(define (img-rectangular z)
  (cdr z))

(define (mag-rectangular z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (ang-rectangular z)
  (atan (cdr z) (car z)))
(define (make-pol r a)
  (attach-type 'polar (cons r a)))

(define (mag-polar z)
  (car z))

(define (ang-polar z)
  (cdr z))

(define (real-polar z)
  (* (car z)
     (cos (cdr z))))

(define (img-polar z)
  (* (car z)
     (sin (cdr z))))
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(put 'rectangular 'real real-rectangular)
(put 'rectangular 'img img-rectangular)
(put 'rectangular 'mag mag-rectangular)
(put 'rectangular 'ang ang-rectangular)
(put 'polar 'real real-polar)
(put 'polar 'img img-polar)
(put 'polar 'mag mag-polar)
(put 'polar 'ang ang-polar)
(define (operate op object)
  (let ((proc (get (type object) op)))
    (if (not (null? proc))
        (proc (contents object))
        (error "Operation undefined."))))
(define (real obj)
  (operate 'real obj))

(define (img obj)
  (operate 'img obj))

(define (mag obj)
  (operate 'mag obj))

(define (ang obj)
  (operate 'ang obj))
(define (+c z1 z2)
  (make-rect
   (+ (real z1) (real z2))
   (+ (img z1) (img z2))))

(define (-c z1 z2)
  (make-rect
   (- (real z1) (real z2))
   (- (img z1) (img z2))))

(define (*c z1 z2)
  (make-pol
   (* (mag z1) (mag z2))
   (+ (ang z1) (ang z2))))

(define (/c z1 z2)
  (make-pol
   (/ (mag z1) (mag z2))
   (- (ang z1) (ang z2))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (+rat x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))
(define (make-rat n d)
  (attach-type 'rational (cons n d)))

(put 'rational 'add +rat)
(put 'rational 'mul *rat)
(define (make-complex z)
  (attach-type 'complex z))

(define (+complex z1 z2)
  (make-complex (+c z1 z2)))

(define (-complex z1 z2)
  (make-complex (-c z1 z2)))

(define (*complex z1 z2)
  (make-complex (*c z1 z2)))

(define (/complex z1 z2)
  (make-complex (/c z1 z2)))

(put 'complex 'add +complex)
(put 'complex 'sub -complex)
(put 'complex 'mul *complex)
(put 'complex 'div /complex)
(define (make-number n)
  (attach-type 'number n))

(define (+number x y)
  (make-number (+ x y)))

(define (-number x y)
  (make-number (- x y)))

(define (*number x y)
  (make-number (* x y)))

(define (/number x y)
  (make-number (/ x y)))

(put 'number 'add +number)
(put 'number 'sub -number)
(put 'number 'mul *number)
(put 'number 'div /number)
(define (operate-2 op arg1 arg2)
  (if (eq? (type arg1) (type arg2))
      (let ((proc (get (type arg1) op)))
        (if (not (null? proc))
            (proc (contents arg1)
                  (contents arg2))
            (error "Operation undefined.")))
      (error "Argument type mismatch.")))
(define (add x y)
  (operate-2 'add x y))

(define (sub x y)
  (operate-2 'sub x y))

(define (mul x y)
  (operate-2 'mul x y))

(define (div x y)
  (operate-2 'div x y))

(define p (make-complex (make-pol 1 2)))
(define q (make-complex (make-pol 3 4)))
(mul q p)

(define r (make-rat 2 4))
(define s (make-rat 1 4))
(add r s)

(sub (make-number 65) (make-number 3))
;; Usage:1 ends here

;; [[file:video_notes.org::asn-poly-cons][asn-poly-cons]]
(define (make-poly var term-list)
  (attach-type 'polynomial (cons var term-list)))

(define (var poly) (car poly))
(define (term-list poly) (cdr poly))
;; asn-poly-cons ends here

;; [[file:video_notes.org::asn-+poly][asn-+poly]]
(define (+poly p1 p2)
  (if (eq? (var p1) (var p2))
      (make-poly
       (var p1)
       (+term (term-list p1)
              (term-list p2)))
      (error "Polynomials not in same variable.")))

(put 'polynomial 'add +poly)
;; asn-+poly ends here

;; [[file:video_notes.org::asn-+term][asn-+term]]
(define (+term L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1
                   (+term (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2
                   (+term L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1)
                                   (coeff t2)))
                   (+term (rest-terms L1) (rest-terms L2)))))))))
;; asn-+term ends here

;; [[file:video_notes.org::asn-tl-cons][asn-tl-cons]]
(define (empty-termlist? tl)
  (null? tl))

(define (first-term tl)
  (car tl))

(define (rest-terms tl)
  (cdr tl))

(define (adjoin-term term tl)
  (cons term tl))

(define (make-term o c)
  (cons o c))

(define (order t)
  (car t))

(define (coeff t)
  (cdr t))
;; asn-tl-cons ends here

;; [[file:video_notes.org::*Adding Polynomials][Adding Polynomials:5]]
(define (square x)
  (* x x))
(define (attach-type type contents)
  (cons type contents))

(define (type datum)
  (car datum))

(define (contents datum)
  (cdr datum))
(define (make-rect x y)
  (attach-type 'rectangular (cons x y)))

(define (real-rectangular z)
  (car z))

(define (img-rectangular z)
  (cdr z))

(define (mag-rectangular z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (ang-rectangular z)
  (atan (cdr z) (car z)))
(define (make-pol r a)
  (attach-type 'polar (cons r a)))

(define (mag-polar z)
  (car z))

(define (ang-polar z)
  (cdr z))

(define (real-polar z)
  (* (car z)
     (cos (cdr z))))

(define (img-polar z)
  (* (car z)
     (sin (cdr z))))
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(put 'rectangular 'real real-rectangular)
(put 'rectangular 'img img-rectangular)
(put 'rectangular 'mag mag-rectangular)
(put 'rectangular 'ang ang-rectangular)
(put 'polar 'real real-polar)
(put 'polar 'img img-polar)
(put 'polar 'mag mag-polar)
(put 'polar 'ang ang-polar)
(define (operate op object)
  (let ((proc (get (type object) op)))
    (if (not (null? proc))
        (proc (contents object))
        (error "Operation undefined."))))
(define (real obj)
  (operate 'real obj))

(define (img obj)
  (operate 'img obj))

(define (mag obj)
  (operate 'mag obj))

(define (ang obj)
  (operate 'ang obj))
(define (+c z1 z2)
  (make-rect
   (+ (real z1) (real z2))
   (+ (img z1) (img z2))))

(define (-c z1 z2)
  (make-rect
   (- (real z1) (real z2))
   (- (img z1) (img z2))))

(define (*c z1 z2)
  (make-pol
   (* (mag z1) (mag z2))
   (+ (ang z1) (ang z2))))

(define (/c z1 z2)
  (make-pol
   (/ (mag z1) (mag z2))
   (- (ang z1) (ang z2))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (+rat x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))
(define (make-rat n d)
  (attach-type 'rational (cons n d)))

(put 'rational 'add +rat)
(put 'rational 'mul *rat)
(define (make-complex z)
  (attach-type 'complex z))

(define (+complex z1 z2)
  (make-complex (+c z1 z2)))

(define (-complex z1 z2)
  (make-complex (-c z1 z2)))

(define (*complex z1 z2)
  (make-complex (*c z1 z2)))

(define (/complex z1 z2)
  (make-complex (/c z1 z2)))

(put 'complex 'add +complex)
(put 'complex 'sub -complex)
(put 'complex 'mul *complex)
(put 'complex 'div /complex)
(define (make-number n)
  (attach-type 'number n))

(define (+number x y)
  (make-number (+ x y)))

(define (-number x y)
  (make-number (- x y)))

(define (*number x y)
  (make-number (* x y)))

(define (/number x y)
  (make-number (/ x y)))

(put 'number 'add +number)
(put 'number 'sub -number)
(put 'number 'mul *number)
(put 'number 'div /number)
(define (operate-2 op arg1 arg2)
  (if (eq? (type arg1) (type arg2))
      (let ((proc (get (type arg1) op)))
        (if (not (null? proc))
            (proc (contents arg1)
                  (contents arg2))
            (error "Operation undefined.")))
      (error "Argument type mismatch.")))
(define (add x y)
  (operate-2 'add x y))

(define (sub x y)
  (operate-2 'sub x y))

(define (mul x y)
  (operate-2 'mul x y))

(define (div x y)
  (operate-2 'div x y))
(define (make-poly var term-list)
  (attach-type 'polynomial (cons var term-list)))

(define (var poly) (car poly))
(define (term-list poly) (cdr poly))
(define (empty-termlist? tl)
  (null? tl))

(define (first-term tl)
  (car tl))

(define (rest-terms tl)
  (cdr tl))

(define (adjoin-term term tl)
  (cons term tl))

(define (make-term o c)
  (cons o c))

(define (order t)
  (car t))

(define (coeff t)
  (cdr t))
(define (+term L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1
                   (+term (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2
                   (+term L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1)
                                   (coeff t2)))
                   (+term (rest-terms L1) (rest-terms L2)))))))))
(define (+poly p1 p2)
  (if (eq? (var p1) (var p2))
      (make-poly
       (var p1)
       (+term (term-list p1)
              (term-list p2)))
      (error "Polynomials not in same variable.")))

(put 'polynomial 'add +poly)

(define p1-tl
  (list
   (cons 15 (make-number 1))
   (cons 7 (make-number 2))
   (cons 0 (make-number 5))))
(define poly1 (make-poly 'x p1-tl))

(define p2-tl
  (list
   (cons 25 (make-number 1))
   (cons 15 (make-number 8))
   (cons 9 (make-number 4))
   (cons 7 (make-number 4))
   (cons 0 (make-number 15))))
(define poly2 (make-poly 'x p2-tl))

(add poly1 poly2)
;; Adding Polynomials:5 ends here

;; [[file:video_notes.org::*Recursive Data Directed Programming][Recursive Data Directed Programming:1]]
(+ (coeff t1)
   (coeff t2))
;; Recursive Data Directed Programming:1 ends here

;; [[file:video_notes.org::*Recursive Data Directed Programming][Recursive Data Directed Programming:2]]
(add (coeff t1)
     (coeff t2))
;; Recursive Data Directed Programming:2 ends here

;; [[file:video_notes.org::*Recursive Data Directed Programming][Recursive Data Directed Programming:3]]
(define (square x)
  (* x x))
(define (attach-type type contents)
  (cons type contents))

(define (type datum)
  (car datum))

(define (contents datum)
  (cdr datum))
(define (make-rect x y)
  (attach-type 'rectangular (cons x y)))

(define (real-rectangular z)
  (car z))

(define (img-rectangular z)
  (cdr z))

(define (mag-rectangular z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (ang-rectangular z)
  (atan (cdr z) (car z)))
(define (make-pol r a)
  (attach-type 'polar (cons r a)))

(define (mag-polar z)
  (car z))

(define (ang-polar z)
  (cdr z))

(define (real-polar z)
  (* (car z)
     (cos (cdr z))))

(define (img-polar z)
  (* (car z)
     (sin (cdr z))))
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(put 'rectangular 'real real-rectangular)
(put 'rectangular 'img img-rectangular)
(put 'rectangular 'mag mag-rectangular)
(put 'rectangular 'ang ang-rectangular)
(put 'polar 'real real-polar)
(put 'polar 'img img-polar)
(put 'polar 'mag mag-polar)
(put 'polar 'ang ang-polar)
(define (operate op object)
  (let ((proc (get (type object) op)))
    (if (not (null? proc))
        (proc (contents object))
        (error "Operation undefined."))))
(define (real obj)
  (operate 'real obj))

(define (img obj)
  (operate 'img obj))

(define (mag obj)
  (operate 'mag obj))

(define (ang obj)
  (operate 'ang obj))
(define (+c z1 z2)
  (make-rect
   (+ (real z1) (real z2))
   (+ (img z1) (img z2))))

(define (-c z1 z2)
  (make-rect
   (- (real z1) (real z2))
   (- (img z1) (img z2))))

(define (*c z1 z2)
  (make-pol
   (* (mag z1) (mag z2))
   (+ (ang z1) (ang z2))))

(define (/c z1 z2)
  (make-pol
   (/ (mag z1) (mag z2))
   (- (ang z1) (ang z2))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (+rat x y)
  (make-rat
   (+ (* (numer x) (denom y))
      (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))
(define (make-rat n d)
  (attach-type 'rational (cons n d)))

(put 'rational 'add +rat)
(put 'rational 'mul *rat)
(define (make-complex z)
  (attach-type 'complex z))

(define (+complex z1 z2)
  (make-complex (+c z1 z2)))

(define (-complex z1 z2)
  (make-complex (-c z1 z2)))

(define (*complex z1 z2)
  (make-complex (*c z1 z2)))

(define (/complex z1 z2)
  (make-complex (/c z1 z2)))

(put 'complex 'add +complex)
(put 'complex 'sub -complex)
(put 'complex 'mul *complex)
(put 'complex 'div /complex)
(define (make-number n)
  (attach-type 'number n))

(define (+number x y)
  (make-number (+ x y)))

(define (-number x y)
  (make-number (- x y)))

(define (*number x y)
  (make-number (* x y)))

(define (/number x y)
  (make-number (/ x y)))

(put 'number 'add +number)
(put 'number 'sub -number)
(put 'number 'mul *number)
(put 'number 'div /number)
(define (operate-2 op arg1 arg2)
  (if (eq? (type arg1) (type arg2))
      (let ((proc (get (type arg1) op)))
        (if (not (null? proc))
            (proc (contents arg1)
                  (contents arg2))
            (error "Operation undefined.")))
      (error "Argument type mismatch.")))
(define (add x y)
  (operate-2 'add x y))

(define (sub x y)
  (operate-2 'sub x y))

(define (mul x y)
  (operate-2 'mul x y))

(define (div x y)
  (operate-2 'div x y))
(define (make-poly var term-list)
  (attach-type 'polynomial (cons var term-list)))

(define (var poly) (car poly))
(define (term-list poly) (cdr poly))
(define (empty-termlist? tl)
  (null? tl))

(define (first-term tl)
  (car tl))

(define (rest-terms tl)
  (cdr tl))

(define (adjoin-term term tl)
  (cons term tl))

(define (make-term o c)
  (cons o c))

(define (order t)
  (car t))

(define (coeff t)
  (cdr t))
(define (+term L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1
                   (+term (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2
                   (+term L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1)
                                   (coeff t2)))
                   (+term (rest-terms L1) (rest-terms L2)))))))))
(define (+poly p1 p2)
  (if (eq? (var p1) (var p2))
      (make-poly
       (var p1)
       (+term (term-list p1)
              (term-list p2)))
      (error "Polynomials not in same variable.")))

(put 'polynomial 'add +poly)

(define p1-tl
  (list
   (cons 15 (make-rat 1 2))
   (cons 7 (make-rat 2 17))
   (cons 0 (make-rat 5 4))))
(define poly1 (make-poly 'x p1-tl))

(define p2-tl
  (list
   (cons 25 (make-rat 1 3))
   (cons 15 (make-rat 8 7))
   (cons 9 (make-rat 4 13))
   (cons 7 (make-rat 14 7))
   (cons 0 (make-rat 15 1))))
(define poly2 (make-poly 'x p2-tl))

(add poly1 poly2)
;; Recursive Data Directed Programming:3 ends here

;; [[file:video_notes.org::*Functional Programming][Functional Programming:1]]
(define (fact n)
  (cond ((= n 1) 1)
        (else (* n (fact (dec n))))))

(fact 4)
;; Functional Programming:1 ends here

;; [[file:video_notes.org::*Functional Programming][Functional Programming:2]]
(fact 4)
(* 4 (fact 3))
(* 4 (* 3 (fact 2)))
(* 4 (* 3 (* 2 (fact 1))))
(* 4 (* 3 (* 2 1)))
(* 4 (* 3 2))
(* 4 6)
24
;; Functional Programming:2 ends here

;; [[file:video_notes.org::*Functional Programming][Functional Programming:3]]
(define (pa+ x y)
  (if (= x 0)
      y
      (pa+ (dec x) (inc y))))
;; Functional Programming:3 ends here

;; [[file:video_notes.org::*Functional Programming][Functional Programming:4]]
(define (pb+ x y)
  (if (= x 0)
      y
      (inc (pb+ (dec x) y))))
;; Functional Programming:4 ends here

;; [[file:video_notes.org::*Imperative Programming][Imperative Programming:1]]
(set! <var> <value>)
;; Imperative Programming:1 ends here

;; [[file:video_notes.org::*Imperative Programming][Imperative Programming:2]]
(define count 1)

(define (demo x)
  (set! count (inc count))
  (+ x count))

(demo 3)
(demo 3)
(demo 3)
;; Imperative Programming:2 ends here

;; [[file:video_notes.org::*Direct Comparison][Direct Comparison:1]]
(define (fact n)
  (define (iter m i)
    (cond ((> i n) m)
          (else
           (iter (* i m) (inc i)))))
  (iter 1 1))

(fact 4)
;; Direct Comparison:1 ends here

;; [[file:video_notes.org::*Direct Comparison][Direct Comparison:2]]
(define (fact n)
  (let ((i 1) (m 1))
    (define (loop)
      (cond ((> i n) m)
            (else
             (set! m (* i m))
             (set! i (inc i))
             (loop))))
    (loop)))

(fact 4)
;; Direct Comparison:2 ends here

;; [[file:video_notes.org::*Direct Comparison][Direct Comparison:3]]
(set! i (inc i))
(set! m (* i m))
;; Direct Comparison:3 ends here

;; [[file:video_notes.org::*Bound and Free Variables][Bound and Free Variables:1]]
(lambda (y) ((lambda (x) (* x y)) 3))
;; Bound and Free Variables:1 ends here

;; [[file:video_notes.org::*Bound and Free Variables][Bound and Free Variables:2]]
(lambda (v) ((lambda (w) (* w v)) 3))
;; Bound and Free Variables:2 ends here

;; [[file:video_notes.org::*Bound and Free Variables][Bound and Free Variables:3]]
(lambda (x) (* x y))
;; Bound and Free Variables:3 ends here

;; [[file:video_notes.org::*Bound and Free Variables][Bound and Free Variables:4]]
(lambda (y) ((lambda (x) (* x y)) 3))
;; Bound and Free Variables:4 ends here

;; [[file:video_notes.org::env-mod-make-counter][env-mod-make-counter]]
(define make-counter
  (lambda (n)
    (lambda ()
      (set! n (inc n))
      n)))
;; env-mod-make-counter ends here

;; [[file:video_notes.org::env-mod-make-counter-inc][env-mod-make-counter-inc]]
(define make-counter
  (lambda (n)
    (lambda ()
      (set! n (inc n))
      n)))
(define C1 (make-counter 0))
(define C2 (make-counter 10))
;; env-mod-make-counter-inc ends here

;; [[file:video_notes.org::*Assignment][Assignment:3]]
(define make-counter
  (lambda (n)
    (lambda ()
      (set! n (inc n))
      n)))
(define C1 (make-counter 0))
(define C2 (make-counter 10))
(C1)
(C2)
(C1)
(C2)
;; Assignment:3 ends here

;; [[file:video_notes.org::c-mcarlo-1][c-mcarlo-1]]
(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))

(define (cesaro)
  (= (gcd (random 4294967087) (random 4294967087)) 1))

(define (monte-carlo trials experiment)
  (define (iter remaining passed)
    (cond ((= remaining 0)
           (/ passed trials))
          ((experiment)
           (iter (dec remaining)
                 (inc passed)))
          (else
           (iter (dec remaining)
                 passed))))
  (iter trials 0))

(estimate-pi 10000000)
;; c-mcarlo-1 ends here

;; [[file:video_notes.org::*Cesàro's Pi Finder][Cesàro's Pi Finder:2]]
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
;; Cesàro's Pi Finder:2 ends here

;; [[file:video_notes.org::*Functional Cesàro's Pi Finder][Functional Cesàro's Pi Finder:1]]
(define (estimate-pi n)
  (sqrt (/ 6 (random-gcd-test n))))

(define (random-gcd-test trials)
  (define (iter remaining passed x)
    (let ((x1 (rand-update x))
          (x2 (rand-update x1)))
      (cond ((= remaining 0)
             (/ passed trials))
            ((= (gcd x1 x2) 1)
             (iter (dec remaining)
                   (inc passed)
                   x))
            (else
             (iter (dec remaining)
                   passed
                   x2)))))
  (iter trials 0 random-seed))
;; Functional Cesàro's Pi Finder:1 ends here

;; [[file:video_notes.org::*Digital Circuit Language][Digital Circuit Language:1]]
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)
;; Digital Circuit Language:1 ends here

;; [[file:video_notes.org::dcl-g7][dcl-g7]]
(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'OK))
;; dcl-g7 ends here

;; [[file:video_notes.org::dcl-g8][dcl-g8]]
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))
;; dcl-g8 ends here

;; [[file:video_notes.org::dcl-g1][dcl-g1]]
(define (inverter in out)
  (define (invert-in)
    (let ((new
           (logical-not (get-signal in))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! out new)))))
  (add-action! in invert-in))
;; dcl-g1 ends here

;; [[file:video_notes.org::dcl-g2][dcl-g2]]
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else
         (error "Invalid signal" s))))
;; dcl-g2 ends here

;; [[file:video_notes.org::dcl-g3][dcl-g3]]
(define (and-gate a1 a2 out)
  (define (and-action-proc)
    (let ((new-value
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! out new-value)))))
  (add-action! a1 and-action-proc)
  (add-action! a2 and-action-proc))
;; dcl-g3 ends here

;; [[file:video_notes.org::dcl-g4][dcl-g4]]
(define (logical-and a b)
  (cond ((= a 0)
         (cond ((= b 0) 0)
               ((= b 1) 0)))
        ((= a 1)
         (cond ((= b 0) 0)
               ((= b 1) 1)))))
;; dcl-g4 ends here

;; [[file:video_notes.org::dcl-g5][dcl-g5]]
(define (or-gate r1 r2 out)
  (define (or-action-proc)
    (let ((new-value
           (logical-or (get-signal r1)
                       (get-signal r2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! out new-value)))))
  (add-action! r1 or-action-proc)
  (add-action! r2 or-action-proc))
;; dcl-g5 ends here

;; [[file:video_notes.org::dcl-g6][dcl-g6]]
(define (logical-or a b)
  (cond ((= a 0)
         (cond ((= b 0) 0)
               ((= b 1) 1)))
        ((= a 1)
         (cond ((= b 0) 1)
               ((= b 1) 1)))))
;; dcl-g6 ends here

;; [[file:video_notes.org::dcl-w1][dcl-w1]]
(define (make-wire)
  (let ((signal 0)
        (action-procs '()))
    (define (set-my-signal! new)
      (cond ((= signal new) 'DONE)
            (else
             (set! signal new)
             (call-each action-procs))))
    (define (accept-action-proc proc)
      (set! action-procs
            (cons proc action-procs))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-proc)
            (else
             (error "Bad message" m))))
    dispatch))
;; dcl-w1 ends here

;; [[file:video_notes.org::dcl-w2][dcl-w2]]
(define (call-each procs)
  (cond ((null? procs) 'DONE)
        (else
         ((car procs))
         (call-each (cdr procs)))))
;; dcl-w2 ends here

;; [[file:video_notes.org::dcl-w3][dcl-w3]]
(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-val)
  ((wire 'set-signal!) new-val))

(define (add-action! wire proc)
  ((wire 'add-action!) proc))
;; dcl-w3 ends here

;; [[file:video_notes.org::dcl-w4][dcl-w4]]
(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))
;; dcl-w4 ends here

;; [[file:video_notes.org::dcl-w5][dcl-w5]]
(define (propagate)
  (cond ((empty-agenda? the-agenda) 'DONE)
        (else
         ((first-agenda-item the-agenda))
         (remove-first-item! the-agenda)
         (propagate))))
;; dcl-w5 ends here

;; [[file:video_notes.org::dcl-a1][dcl-a1]]
(define (make-time-segment time q)
  (cons time q))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))
;; dcl-a1 ends here

;; [[file:video_notes.org::dcl-a2][dcl-a2]]
(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))
;; dcl-a2 ends here

;; [[file:video_notes.org::dcl-a3][dcl-a3]]
(define (empty-agenda? agenda)
  (null? (segments agenda)))
;; dcl-a3 ends here

;; [[file:video_notes.org::dcl-a4][dcl-a4]]
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))
;; dcl-a4 ends here

;; [[file:video_notes.org::dcl-a5][dcl-a5]]
(define (remove-first-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))
;; dcl-a5 ends here

;; [[file:video_notes.org::dcl-a6][dcl-a6]]
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty --- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))
;; dcl-a6 ends here

;; [[file:video_notes.org::dcl-q1][dcl-q1]]
(define (make-queue) (cons '() '()))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT: empty queue" queue)
      (car (front-ptr queue))))
;; dcl-q1 ends here

;; [[file:video_notes.org::dcl-q2][dcl-q2]]
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
;; dcl-q2 ends here

;; [[file:video_notes.org::dcl-q3][dcl-q3]]
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE-QUEUE!: called with empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))
;; dcl-q3 ends here

;; [[file:video_notes.org::dcl-p][dcl-p]]
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name)
                 (display ": TIME = ")
                 (display (current-time the-agenda))
                 (display ": NEW-VAL = ")
                 (display (get-signal wire))
                 (newline))))
;; dcl-p ends here

;; [[file:video_notes.org::*Using the Digital Circuit Language][Using the Digital Circuit Language:2]]
(define (make-queue) (cons '() '()))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT: empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE-QUEUE!: called with empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))
(define (make-time-segment time q)
  (cons time q))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))
(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))
(define (remove-first-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty --- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))
(define (make-wire)
  (let ((signal 0)
        (action-procs '()))
    (define (set-my-signal! new)
      (cond ((= signal new) 'DONE)
            (else
             (set! signal new)
             (call-each action-procs))))
    (define (accept-action-proc proc)
      (set! action-procs
            (cons proc action-procs))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-proc)
            (else
             (error "Bad message" m))))
    dispatch))
(define (call-each procs)
  (cond ((null? procs) 'DONE)
        (else
         ((car procs))
         (call-each (cdr procs)))))
(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-val)
  ((wire 'set-signal!) new-val))

(define (add-action! wire proc)
  ((wire 'add-action!) proc))
(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))
(define (propagate)
  (cond ((empty-agenda? the-agenda) 'DONE)
        (else
         ((first-agenda-item the-agenda))
         (remove-first-item! the-agenda)
         (propagate))))
(define (inverter in out)
  (define (invert-in)
    (let ((new
           (logical-not (get-signal in))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! out new)))))
  (add-action! in invert-in))
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else
         (error "Invalid signal" s))))
(define (and-gate a1 a2 out)
  (define (and-action-proc)
    (let ((new-value
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! out new-value)))))
  (add-action! a1 and-action-proc)
  (add-action! a2 and-action-proc))
(define (logical-and a b)
  (cond ((= a 0)
         (cond ((= b 0) 0)
               ((= b 1) 0)))
        ((= a 1)
         (cond ((= b 0) 0)
               ((= b 1) 1)))))
(define (or-gate r1 r2 out)
  (define (or-action-proc)
    (let ((new-value
           (logical-or (get-signal r1)
                       (get-signal r2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! out new-value)))))
  (add-action! r1 or-action-proc)
  (add-action! r2 or-action-proc))
(define (logical-or a b)
  (cond ((= a 0)
         (cond ((= b 0) 0)
               ((= b 1) 1)))
        ((= a 1)
         (cond ((= b 0) 1)
               ((= b 1) 1)))))
(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'OK))
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name)
                 (display ": TIME = ")
                 (display (current-time the-agenda))
                 (display ": NEW-VAL = ")
                 (display (get-signal wire))
                 (newline))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'SUM sum)
(probe 'CAR carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)
;; Using the Digital Circuit Language:2 ends here

;; [[file:video_notes.org::*Church's =set-car!= and =set-cdr!=][Church's =set-car!= and =set-cdr!=:1]]
(define (cons x y)
  (lambda (m)
    (m x y)))

(define (car x)
  (x (lambda (a d) a)))

(define (cdr x)
  (x (lambda (a d) d)))

(define a (cons 1 2))
(car a)
(cdr a)
;; Church's =set-car!= and =set-cdr!=:1 ends here

;; [[file:video_notes.org::*Church's =set-car!= and =set-cdr!=][Church's =set-car!= and =set-cdr!=:2]]
(define a (cons 1 2))
(define a (lambda (m) (m 1 2)))

(car a)
(car (lambda (m) (m 1 2)))
((lambda (a d) a) 1 2)
1
;; Church's =set-car!= and =set-cdr!=:2 ends here

;; [[file:video_notes.org::*Church's =set-car!= and =set-cdr!=][Church's =set-car!= and =set-cdr!=:3]]
(define (cons x y)
  (lambda (m)
    (m x
       y
       (lambda (n) (set! x n))
       (lambda (n) (set! y n)))))

(define (car x)
  (x (lambda (a d sa sd) a)))

(define (cdr x)
  (x (lambda (a d sa sd) d)))

(define (set-car! x n)
  (x (lambda (a d sa sd) (sa n))))

(define (set-cdr! x n)
  (x (lambda (a d sa sd) (sd n))))

(define a (cons 1 2))
(car a) (cdr a)
(set-car! a 10)
(set-cdr! a 20)
(car a) (cdr a)
;; Church's =set-car!= and =set-cdr!=:3 ends here

;; [[file:video_notes.org::str-1-tree][str-1-tree]]
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (make-tree left right)
  (list left right))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (leaf-node? node)
  (atom? node))
;; str-1-tree ends here

;; [[file:video_notes.org::*Stream Processing][Stream Processing:2]]
(define (square x)
  (* x x))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (make-tree left right)
  (list left right))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (leaf-node? node)
  (atom? node))

(define (sum-odd-squares tree)
  (if (leaf-node? tree)
      (if (odd? tree)
          (square tree)
          0)
      (+ (sum-odd-squares
          (left-branch tree))
         (sum-odd-squares
          (right-branch tree)))))

(define my-tree (make-tree (make-tree 1 (make-tree 2 7)) (make-tree 13 (make-tree 12 14))))
(sum-odd-squares my-tree)
;; Stream Processing:2 ends here

;; [[file:video_notes.org::*Stream Processing][Stream Processing:3]]
(define (iter-fib n a b)
  (if (= n 1)
      b
      (iter-fib (dec n) b (+ a b))))

(define (fib n)
  (iter-fib n 0 1))
(define (odd-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (odd? f)
              (cons f (next (inc k)))
              (next (inc k))))))
  (next 1))

(odd-fibs 10)
;; Stream Processing:3 ends here

;; [[file:video_notes.org::str-1-macros][str-1-macros]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
;; str-1-macros ends here

;; [[file:video_notes.org::str-1-seqs][str-1-seqs]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))

(define (force p)
  (p))

(define (empty-stream? s)
  (null? s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))
;; str-1-seqs ends here

;; [[file:video_notes.org::*Stream Processing][Stream Processing:6]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))

(define (force p)
  (p))

(define (empty-stream? s)
  (null? s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))

(define x 1)
(define y 2)
(head (cons-stream x y))
(tail (cons-stream x y))
;; Stream Processing:6 ends here

;; [[file:video_notes.org::str-1-map-filter-accum-append-enum-enum-tree][str-1-map-filter-accum-append-enum-enum-tree]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))

(define (force p)
  (p))

(define (empty-stream? s)
  (null? s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (make-tree left right)
  (list left right))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (leaf-node? node)
  (atom? node))
(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter-stream pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter-stream pred (tail s))))
    (else (filter-stream pred (tail s)))))

(define (accumulate-stream combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate-stream combiner init-val (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (enumerate-interval (inc low) high))))

(define (singleton s)
  (cons-stream s the-empty-stream))
;; str-1-map-filter-accum-append-enum-enum-tree ends here

;; [[file:video_notes.org::*Stream Processing][Stream Processing:8]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))

(define (force p)
  (p))

(define (empty-stream? s)
  (null? s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (make-tree left right)
  (list left right))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (leaf-node? node)
  (atom? node))
(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter-stream pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter-stream pred (tail s))))
    (else (filter-stream pred (tail s)))))

(define (accumulate-stream combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate-stream combiner init-val (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (enumerate-interval (inc low) high))))

(define (singleton s)
  (cons-stream s the-empty-stream))
(define (iter-fib n a b)
  (if (= n 1)
      b
      (iter-fib (dec n) b (+ a b))))

(define (fib n)
  (iter-fib n 0 1))
(define (square x)
  (* x x))
(define (sum-odd-squares tree)
  (accumulate-stream +
                     0
                     (map-stream square
                                 (filter-stream odd?
                                                (enumerate-tree tree)))))

(define (odd-fibs n)
  (accumulate-stream cons
                     '()
                     (filter-stream odd?
                                    (map-stream fib
                                                (enumerate-interval 1 n)))))

(define my-tree (make-tree (make-tree 1 (make-tree 2 7)) (make-tree 13 (make-tree 12 14))))
(sum-odd-squares my-tree)

(odd-fibs 10)
;; Stream Processing:8 ends here

;; [[file:video_notes.org::*Flattening][Flattening:1]]
((1 2 3) (4 5 6) (7 8 9))
;; Flattening:1 ends here

;; [[file:video_notes.org::*Flattening][Flattening:2]]
(1 2 3 4 5 6 7 8 9)
;; Flattening:2 ends here

;; [[file:video_notes.org::str-1-flatten][str-1-flatten]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))

(define (force p)
  (p))

(define (empty-stream? s)
  (null? s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (make-tree left right)
  (list left right))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (leaf-node? node)
  (atom? node))
(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter-stream pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter-stream pred (tail s))))
    (else (filter-stream pred (tail s)))))

(define (accumulate-stream combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate-stream combiner init-val (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (enumerate-interval (inc low) high))))

(define (singleton s)
  (cons-stream s the-empty-stream))
(define (flatten stream-of-streams)
  (accumulate-stream append-streams
                     the-empty-stream
                     stream-of-streams))
;; str-1-flatten ends here

;; [[file:video_notes.org::str-1-flatmap][str-1-flatmap]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))

(define (force p)
  (p))

(define (empty-stream? s)
  (null? s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (make-tree left right)
  (list left right))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (leaf-node? node)
  (atom? node))
(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter-stream pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter-stream pred (tail s))))
    (else (filter-stream pred (tail s)))))

(define (accumulate-stream combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate-stream combiner init-val (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (enumerate-interval (inc low) high))))

(define (singleton s)
  (cons-stream s the-empty-stream))
(define (flatten stream-of-streams)
  (accumulate-stream append-streams
                     the-empty-stream
                     stream-of-streams))
(define (flatmap f s)
  (flatten (map-stream f s)))
;; str-1-flatmap ends here

;; [[file:video_notes.org::*Prime Sum Pairs][Prime Sum Pairs:1]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))

(define (force p)
  (p))

(define (empty-stream? s)
  (null? s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (make-tree left right)
  (list left right))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (leaf-node? node)
  (atom? node))
(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter-stream pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter-stream pred (tail s))))
    (else (filter-stream pred (tail s)))))

(define (accumulate-stream combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate-stream combiner init-val (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (enumerate-interval (inc low) high))))

(define (singleton s)
  (cons-stream s the-empty-stream))
(define (flatten stream-of-streams)
  (accumulate-stream append-streams
                     the-empty-stream
                     stream-of-streams))
(define (flatmap f s)
  (flatten (map-stream f s)))

(#%require math/number-theory) ;; for prime?
(define (prime-sum-pairs n)
  (map-stream (lambda (p)
                (list (car p) (cadr p) (+ (car p) (cadr p))))
              (filter-stream (lambda (p)
                               (prime? (+ (car p) (cadr p))))
                             (flatmap (lambda (i)
                                        (map-stream (lambda (j) (list i j))
                                                    (enumerate-interval 1 (dec i))))
                                      (enumerate-interval 1 n)))))

(head (prime-sum-pairs 6))
(head (tail (prime-sum-pairs 6)))
(head (tail (tail (prime-sum-pairs 6))))
;; Prime Sum Pairs:1 ends here

;; [[file:video_notes.org::*The Catch: On Efficiency][The Catch: On Efficiency:1]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))

(define (force p)
  (p))

(define (empty-stream? s)
  (null? s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (make-tree left right)
  (list left right))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (leaf-node? node)
  (atom? node))
(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter-stream pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter-stream pred (tail s))))
    (else (filter-stream pred (tail s)))))

(define (accumulate-stream combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate-stream combiner init-val (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (enumerate-interval (inc low) high))))

(define (singleton s)
  (cons-stream s the-empty-stream))
(#%require math/number-theory)
(define (get-prime low high)
  (filter-stream prime?
                 (enumerate-interval low high)))
(head (tail (get-prime 10000 1000000000000000)))
;; The Catch: On Efficiency:1 ends here

;; [[file:video_notes.org::*Implementing Streams][Implementing Streams:1]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))
;; Implementing Streams:1 ends here

;; [[file:video_notes.org::*Implementing Streams][Implementing Streams:2]]
(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (lambda () expr))))

(define (force p)
  (p))
;; Implementing Streams:2 ends here

;; [[file:video_notes.org::*Implementing Streams][Implementing Streams:3]]
(define (get-prime low high)
  (filter-stream prime?
                 (enumerate-interval low high)))
(head (tail (get-prime 10000 1000000000000000)))
;; Implementing Streams:3 ends here

;; [[file:video_notes.org::*Implementing Streams][Implementing Streams:4]]
(enumerate-interval 10000 1000000000000000)
;; Implementing Streams:4 ends here

;; [[file:video_notes.org::*Implementing Streams][Implementing Streams:5]]
(define (enumerate-interval 10000 1000000000000000)
  (if (> 10000 1000000000000000)
      the-empty-stream
      (cons-stream 10000
                   (enumerate-interval (inc 10000) 1000000000000000))))
;; Implementing Streams:5 ends here

;; [[file:video_notes.org::*Implementing Streams][Implementing Streams:6]]
(cons 10000 (delay (enumerate-interval (inc 10000) 1000000000000000)))
;; Implementing Streams:6 ends here

;; [[file:video_notes.org::*Implementing Streams][Implementing Streams:7]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))

(define (force p)
  (p))

(define (empty-stream? s)
  (null? s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (make-tree left right)
  (list left right))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (leaf-node? node)
  (atom? node))
(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter-stream pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter-stream pred (tail s))))
    (else (filter-stream pred (tail s)))))

(define (accumulate-stream combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate-stream combiner init-val (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (enumerate-interval (inc low) high))))

(define (singleton s)
  (cons-stream s the-empty-stream))
(cons 10000 (lambda () (enumerate-interval (inc 10000) 1000000000000000)))
;; Implementing Streams:7 ends here

;; [[file:video_notes.org::*Implementing Streams][Implementing Streams:8]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))

(define (force p)
  (p))

(define (empty-stream? s)
  (null? s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (make-tree left right)
  (list left right))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (leaf-node? node)
  (atom? node))
(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter-stream pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter-stream pred (tail s))))
    (else (filter-stream pred (tail s)))))

(define (accumulate-stream combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate-stream combiner init-val (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (enumerate-interval (inc low) high))))

(define (singleton s)
  (cons-stream s the-empty-stream))
(head (cons 10000 (lambda () (enumerate-interval (inc 10000) 1000000000000000))))
;; Implementing Streams:8 ends here

;; [[file:video_notes.org::*Implementing Streams][Implementing Streams:9]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))

(define (force p)
  (p))

(define (empty-stream? s)
  (null? s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (make-tree left right)
  (list left right))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (leaf-node? node)
  (atom? node))
(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter-stream pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter-stream pred (tail s))))
    (else (filter-stream pred (tail s)))))

(define (accumulate-stream combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate-stream combiner init-val (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (enumerate-interval (inc low) high))))

(define (singleton s)
  (cons-stream s the-empty-stream))
(tail (cons 10000 (lambda () (enumerate-interval (inc 10000) 1000000000000000))))
;; Implementing Streams:9 ends here

;; [[file:video_notes.org::*Implementing Streams][Implementing Streams:10]]
(define (filter-stream pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter-stream pred (tail s))))
    (else (filter-stream pred (tail s)))))
;; Implementing Streams:10 ends here

;; [[file:video_notes.org::*Implementing Streams][Implementing Streams:11]]
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a e) (cons a (delay e)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
(define (head s)
  (car s))

(define (tail s)
  (force (cdr s)))

(define (force p)
  (p))

(define (empty-stream? s)
  (null? s))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (make-tree left right)
  (list left right))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (leaf-node? node)
  (atom? node))
(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (proc (head s))
       (map-stream proc (tail s)))))

(define (filter-stream pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter-stream pred (tail s))))
    (else (filter-stream pred (tail s)))))

(define (accumulate-stream combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate-stream combiner init-val (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (enumerate-interval (inc low) high))))

(define (singleton s)
  (cons-stream s the-empty-stream))
(#%require math/number-theory)
(filter-stream prime? (cons 10000 (lambda () (enumerate-interval (inc 10000) 1000000000000000))))
;; Implementing Streams:11 ends here

;; [[file:video_notes.org::*A Small Optimization][A Small Optimization:1]]
(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc (lambda () expr)))))
;; A Small Optimization:1 ends here

;; [[file:video_notes.org::*A Small Optimization][A Small Optimization:2]]
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? true)
            result)
          result))))
;; A Small Optimization:2 ends here
