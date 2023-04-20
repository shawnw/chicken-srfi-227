;;; Chicken port of SRFI-227 by Shawn Wagner <shawnw.mobile@mgail.com>

;;; I couldn't get the reference version using syntax-rules macros working
;;; in Chicken, so an ir-macro-transformer version it is.


;; Copyright © 2023 Shawn Wagner <shawnw.mobile@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


(module
 (srfi 227)
 (opt-lambda opt*-lambda let-optionals let-optionals*)
 (import scheme
         (chicken base)
         (only (chicken platform) register-feature!)
         (chicken syntax))

 (register-feature! 'srfi-227)

 (begin-for-syntax
  (import (srfi 1))

  (define (get-identifiers formals)
    (let loop ((formals formals)
               (req-ids '())
               (in-opts #f)
               (opt-ids '())
               (rest-id '()))
      (cond
       ((null? formals)
        (values
         (reverse req-ids)
         (reverse opt-ids)
         rest-id))
       ((symbol? formals)
        (loop '() req-ids #t opt-ids formals))
       ((list? (car formals))
        (loop (cdr formals) req-ids #t (cons (caar formals) opt-ids) rest-id))
       ((symbol? (car formals))
        (if in-opts
            (error "Invalid opt-formal; required after optional" (car formals))
            (loop (cdr formals) (cons (car formals) req-ids) #f opt-ids rest-id)))
       (else
        (error "Invalid opt-formal" (car formals))))))

  (define (make-opt-cases req-ids opt-ids opt-defaults)
    (let loop ((req-ids req-ids)
               (opt-ids opt-ids)
               (opt-defaults opt-defaults)
               (cases '()))
      (if (null? opt-defaults)
          cases
          (loop
           (append req-ids (list (car opt-ids)))
           (cdr opt-ids)
           (cdr opt-defaults)
           (cons (make-opt-case
                  (map gensym req-ids)
                  opt-defaults)
                 cases)))))

  (define (make-opt-case req-ids defaults)
    `((,@req-ids)
      (f ,@req-ids ,@defaults)))

  (define (make-opt*-cases req-ids opt-ids opt-defaults)
    (let loop ((req-ids req-ids)
               (opt-ids opt-ids)
               (opt-defaults opt-defaults)
               (cases '()))
      (if (null? opt-defaults)
          cases
          (loop
           (append req-ids (list (car opt-ids)))
           (cdr opt-ids)
           (cdr opt-defaults)
           (cons (make-opt*-case
                  req-ids
                  (car opt-defaults))
                 cases)))))

  (define (make-opt*-case req-ids default)
    `((,@req-ids)
      (f ,@req-ids ,default))))

 (define-syntax opt-lambda
   (ir-macro-transformer
    (lambda (exp inject compare)
      (let*-values (((formals) (cadr exp))
                    ((body) (cddr exp))
                    ((req-ids opt-ids rest-id) (get-identifiers formals)))

        (if (null? opt-ids)
            `(lambda (,@(append req-ids rest-id)) ,@body) ; no optionals
            `(letrec
                 ((f
                   (case-lambda
                    ((,@(append req-ids opt-ids rest-id)) ; all args given
                     ,@body)
                    ,@(make-opt-cases
                       req-ids
                       opt-ids
                       (map cadr
                            (drop-while symbol? (drop-right formals 0)))))))
               f))))))

 (define-syntax opt*-lambda
   (ir-macro-transformer
    (lambda (exp inject compare)
      (let*-values (((formals) (cadr exp))
                    ((body) (cddr exp))
                    ((req-ids opt-ids rest-id) (get-identifiers formals)))

        (if (null? opt-ids)
            `(lambda (,@(append req-ids rest-id)) ,@body) ; no optionals
            `(letrec
                 ((f
                   (case-lambda
                    ((,@(append req-ids opt-ids rest-id)) ; all args given
                     ,@body)
                    ,@(make-opt*-cases
                       req-ids
                       opt-ids
                       (map cadr
                            (drop-while symbol? (drop-right formals 0)))))))
               f))))))

 (define-syntax let-optionals
   (syntax-rules ()
     ((_ expr opt-formals body1 ... body2)
      (apply (opt-lambda opt-formals body1 ... body2) expr))))

 (define-syntax let-optionals*
   (syntax-rules ()
     ((_ expr opt-formals body1 ... body2)
      (apply (opt*-lambda opt-formals body1 ... body2) expr)))))
