#!/usr/local/bin/csi -s

(require-extension (srfi 227) (srfi 227 definition) test)

(test-begin "srfi-227")

(define f
  (opt*-lambda (x (y 1) (z (* x x)))
    (list x y z)))

(test-group "opt*-lambda"
            (test '(1 2 3) (f 1 2 3))
            (test '(2 3 4) (f 2 3))
            (test '(2 1 4) (f 2)))

(define g
  (let ([x 4])
    (opt-lambda (x (y 1) (z (* x x)))
      (list x y z))))

(test-group "opt-lambda"
            (test '(1 2 3) (g 1 2 3))
            (test '(2 3 16) (g 2 3))
            (test '(2 1 16) (g 2)))

(define h
  (opt-lambda args args))

(test "opt-lambda rest-args" '(1 2) (h 1 2))

(test-group "let-optionals"
            (test '(1 (2))
		  (let-optionals
		   '(1 2)
		   (x . y)
		   (list x y)))
            (test '(1 2 3)
		  (let-optionals
		   '(1)
		   (x (y 2) (z 3))
		   (list x y z))))

(test "let-optionals*" '(1 3 4)
      (let-optionals*
       '(1 3)
       (x (y 2) (z (+ x y)))
       (list x y z)))

(test "define-optionals"
      '(0 1)
      (let* ()
        (define-optionals (f x (y 1))
          (list x y))
        (f 0)))

(test "define-optionals*"
      '(3 9 ())
      (let* ()
        (define-optionals* (f x (y (* x x)) . z)
          (list x y z))
        (f 3)))

(test-end)
(test-exit)
