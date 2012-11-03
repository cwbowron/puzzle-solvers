#lang scheme

(require
 scheme/vector
 (prefix-in srfi: srfi/1)
 srfi/2
 )

(provide (all-defined-out))

(define grid-size (make-parameter 9))

(define universe
  (let ([u (build-list (grid-size) add1)])
    (lambda ()
      (unless (= (grid-size) (length u))
        (set! u (build-list (grid-size) add1)))
      u)))

(define (grid-size-root)
  (sqrt (grid-size)))

(define (make-grid)
  (vector-map (lambda (v) (make-vector (grid-size) #f)) (make-vector (grid-size))))

(define (grid-set! grid m n v)
  (vector-set! (vector-ref grid m) n v))

(define (grid-ref grid m n)
  (vector-ref (vector-ref grid m) n))

(define (remove-false the-list)
  (filter-not false? the-list))

(define (grid-row-contents grid m n)
  (remove-false
   (build-list (grid-size) (lambda (i) (grid-ref grid m i)))))
  
(define (grid-column-contents grid m n)
  (remove-false
   (build-list (grid-size) (lambda (i) (grid-ref grid i n)))))

(define (grid-box-contents grid m n)  
  (let ([r (grid-size-root)])
    (remove-false
     (let ([i (* r (floor (/ n r)))]
           [j (* r (floor (/ m r)))])
       (for*/list ((x (in-range r))
                   (y (in-range r)))
         (grid-ref grid (+ j y) (+ i x)))))))

(define (pretty-print-grid grid)
  (let ([size (grid-size)]
        [border "+---+---+---+---+---+---+---+---+---+~%"])
    (printf border)
    (for ((n (in-range size)))
      (for ((m (in-range size)))
        (printf "| ~A " (or (grid-ref grid n m) " ")))
      (printf "|~%")
      (printf border))))      

(define (possibles grid m n)
  (let ([R (grid-row-contents grid m n)]
        [C (grid-column-contents grid m n)]
        [S (grid-box-contents grid m n)])
    (srfi:lset-difference
     eq?
     (universe)
     (srfi:lset-union 
      eq?
      R C S))))

;(define (grid-full? grid)
;  (for*/and ((n (in-range 9))
;            (m (in-range 9)))
;    (grid-ref grid m n)))

(define SAMPLE-GRID
  (vector 
   (vector #f #f 06 07 09 #f 05 #f #f)
   (vector 07 09 #f 04 #f #f #f #f #f)
   (vector #f 04 #f 03 #f 08 #f #f 07)
   (vector #f 01 #f 09 03 05 06 04 #f)
   (vector #f #f #f 06 #f 04 #f #f #f)
   (vector #f 06 04 02 01 07 #f 03 #f)
   (vector 04 #f #f 08 #f 09 #f 07 #f)
   (vector #f #f #f #f #f 03 #f 05 09)
   (vector #f #f 03 #f 04 06 01 #f #f)))

;;(define (foo grid)
;;  (for ((m (in-range 9)))
;;    (for ((n (in-range 9)))
;;      (unless (grid-ref grid m n)
;;        (printf "P(~A,~A) = ~A~%" m n (possibles grid m n))))))

(define (count-blanks grid)
  (vector-count false? (apply vector-append (vector->list grid))))
      
(define (copy-grid grid)
  (vector-map vector-copy grid))

(define grid-equal? equal?)

(define (solve-one-off grid)
  (let ([copy (copy-grid grid)]
        [modified #f])
    (for* ((m (in-range (grid-size)))
           (n (in-range (grid-size))))
      (and-let* (((not (grid-ref grid m n)))
                 (p (possibles copy m n))
                 ((= 1 (length p))))
        (printf "One Possible: ~A ~A~%" m n)
        (set! modified #t)
        (grid-set! copy m n (first p))))
    (if modified copy grid)))

;(define (target-cell grid)
;  (for*/or ((m (in-range 9))
;            (n (in-range 9)))
;    (and 
;     (not (grid-ref grid m n))
;     (list m n))))

;(define (target-cell grid)
;  (let ([fewest #f]
;        [fewest-indices #f])
;    (for* ((m (in-range 9))
;           (n (in-range 9)))
;      (when (not (grid-ref grid m n))
;        (let ([p (possibles grid m n)])
;          (when (or
;                 (or (not fewest) (< (length p) fewest))
;                 ;;; randomly choose one if they are the same length...
;                 (and (= (length p) fewest) (= 0 (random 2))))
;            (set! fewest (length p))
;            (set! fewest-indices (list m n))))))
;    fewest-indices))

(define (random-element set)
  (list-ref set (random (length set))))

(define (randomize-set set)
  (let loop ([set set]
             [accum (list)])
    (if (empty? set)
        accum
        (let ([item (random-element set)])
          (loop (remove item set) (cons item accum))))))

;;; keep a list of the pairs of indices in case of multiples...
;;; in the end, randomly choose one of the fewest...
(define (target-cell grid)
  (let ([fewest #f]
        [fewest-list (list)])
    (for* ((m (in-range (grid-size)))
           (n (in-range (grid-size))))
      (when (not (grid-ref grid m n))
        (let ([p (possibles grid m n)])
          (cond [(or (not fewest) (< (length p) fewest))
                 (set! fewest (length p))
                 (set! fewest-list (list (list m n)))]
                [(and (= (length p) fewest) (= 0 (random 2)))
                 (set! fewest-list (cons (list m n) fewest-list))]))))
    (and 
     fewest
     (random-element fewest-list))))

(define (solve grid)
  (let ([c (solve-one-off grid)])
    (cond
      [(not (grid-equal? c grid))
       (solve c)]
      [(zero? (count-blanks grid))
       (pretty-print-grid grid)
       grid]
      [else
       (and-let* ([target (target-cell grid)]
                  [m (first target)]
                  [n (second target)]
                  [p (possibles grid m n)])
         (if (zero? (length p))
             (begin0
               #f
               (printf "Dead End~%"))
             (let ([copy (copy-grid grid)])
               (printf "Brute Forcing: ~A ~A: ~A~%" m n p)
               (for/or ((p0 p))
                 (grid-set! copy m n p0)
                 (solve copy)))))])))

(define (solve-2 grid #:update (update #f))
  (when update
    (update grid))
  (cond
    [(zero? (count-blanks grid))
     ;;;(pretty-print-grid grid)
     grid]
    [else
     (and-let* ([target (target-cell grid)]
                [m (first target)]
                [n (second target)]
                [p (possibles grid m n)])
       (if (zero? (length p))
           (begin0
             #f
             ;;;(printf "Dead End~%")
             )
           (let ([copy (copy-grid grid)])
             ;;(cond [(= 1 (length p))
             ;;       (printf "Only One: ~A ~A: ~A~%" m n p)]
             ;;      [else
             ;;       (printf "Brute Forcing: ~A ~A: ~A~%" m n p)])
             (for/or ((p0 (randomize-set p)))
               ;;;(printf "Brute Forcing: ~A ~A: ~A [~A]~%" m n p p0)
               (grid-set! copy m n p0)
               (solve-2 copy #:update update)))))]))
