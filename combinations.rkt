#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define shwoop
	(lambda (lat)
		(cond
			((null? lat) '())
			((atom? lat) lat)
			((shwoop (car lat))))))

(define shweep
	(lambda (lat)
		(cond
			((null? lat) '())
			((atom? (car lat)) (cdr lat))
			((null? (car lat)) (cdr lat))
			(else
				(cons
					(shweep (car lat))
					(cdr lat))))))

(define multiinsert
  (lambda (new lat)
    (cond
      ((null? lat) (cons new '()))
      ((eq? (car lat) old)
       (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertL new old (cdr lat)))))))

(define combinations
	(lambda (lat)
		(cond
			((null? lat) '())
			 )))

(combinations '((1, 2, 3), (4, 5, 6), (7, 8, 9)))
#|
(((1, 2), (3, 4)) ((5, 6), (7, 8)))



|#
