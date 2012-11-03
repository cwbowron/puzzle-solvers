#lang racket

(require srfi/25)

(define *count-find-moves* 0)
(define *count-do-move* 0)

(define (board-set! board c r v)
  (array-set! board c r v))

(define (board-ref board c r)
  (array-ref board c r))

(define (peg? board c r)
  (equal? (array-ref board c r) 'peg))

(define (open? board c r)
  (equal? (array-ref board c r) 'open))

(define (copy-board b)
  (let ([b2 (make-array (shape 0 5 0 5))])
    (for ([c (in-range 5)])
      (for ([r (in-range 5)])
        (board-set! b2 c r (board-ref b c r))))
    b2))

(define (valid? c r)
  (and (< c 5)
       (< r 5)
       (>= c 0)
       (>= r 0)
       (<= c r)))

(define (peg-count board)
  (let ([p 0])
    (for ([c (in-range 5)])
      (for ([r (in-range 5)])
        (when (peg? board c r)
          (set! p (add1 p)))))
    p))
   
(define (initial-board)
  (let ([b (make-array (shape 0 5 0 5))])
    (for ([c (in-range 5)])
      (for ([r (in-range 5)])
        (board-set! b c r 
                    (if (valid? c r) 'peg #f))))
    (board-set! b 1 2 'open)
    ;;;(board-set! b 0 0 'open)
    b))

(define (print-board board)
  (for ([row (in-range 5)])
    (for ([c (in-range (- 4 row))])
      (printf " "))
    (for ([col (in-range (add1 row))])
      (case (board-ref board col row)
        [(peg) (printf " x")]
        [(open) (printf " o")]))
    (printf "~%"))
  (printf "PEGS: ~A~%" (peg-count board)))
  

(define (print-board-debug board)
  (for ([r (in-range 5)])
    (for ([c (in-range 5)])
      (printf "~A\t" (board-ref board c r)))
    (newline)))

(define (valid-move? board c0 r0 c1 r1)
  (let ([cm (quotient (+ c0 c1) 2)]
        [rm (quotient (+ r0 r1) 2)])
    (and (valid? c1 r1)
         (valid? c0 r0)
         (valid? cm rm)
         (open? board c1 r1)
         (peg? board c0 r0)
         (peg? board cm rm))))

(define (find-moves board)
  (set! *count-find-moves* (add1 *count-find-moves*))
  (let ([moves empty])
    (for* ([r0 (in-range 5)]
           [c0 (in-range 5)])
      (when (peg? board c0 r0)
        (for* ([dr (list 0 -2 2)]
               [dc (list 2 -2 0)])
          (unless (and (= dc 0) (= dr 0))
            (let ([c1 (+ c0 dc)]
                  [r1 (+ r0 dr)])
              (when (valid-move? board c0 r0 c1 r1)
                (set! moves
                      (cons (cons (cons c0 r0) (cons c1 r1))
                            moves))))))))
    moves))

(define (do-move board move)
  (set! *count-do-move* (add1 *count-do-move*))
  (let ([p0 (car move)]
        [p1 (cdr move)])
    (let ([c0 (car p0)]
          [r0 (cdr p0)]
          [c1 (car p1)]
          [r1 (cdr p1)])
      (let ([cm (quotient (+ c0 c1) 2)]
            [rm (quotient (+ r0 r1) 2)])
        (let ([b2 (copy-board board)])
          (board-set! b2 c0 r0 'open)
          (board-set! b2 cm rm 'open)
          (board-set! b2 c1 r1 'peg)
          b2)))))
      
(define (solution? board)
  (= (peg-count board) 1))

(define (find-solution board path)
  (define (try-each board moves)
    (for/or ([move moves])
      (find-solution (do-move board move) (cons move path))))
  (let ([moves (find-moves board)])
    (cond
      [(solution? board)
       (reverse path)]
      [(empty? moves) 
       ;;;(printf "BACKUP!~%")
       #f]
      [else 
       (try-each board moves)])))

(define (display-moves board depth path)
  (printf "----------~%")
  (printf "PATH: ~A~%" path)
  (printf "DEPTH: ~A~%" depth)
  (print-board board)
  (when (> depth 0) 
    (let ([moves (find-moves board)])
      (printf "MOVES: ~A~%" moves)
      (for ([m moves])
        (display-moves (do-move board m) (sub1 depth) (cons m path))))))

;;;(define b (initial-board))
;;;(print-board b)
;;;(find-moves b)
;;;(display-moves b 2 empty)

(define (display-move-path board path)
  ;;;(printf "PATH: ~A~%" path)
  (print-board board)
  ;;;(print-board-debug board)
  (unless (empty? path)
    (display-move-path (do-move board (car path)) (cdr path))))

;;;(require racket/trace)
;;;(trace find-solution)

(let* ([board (initial-board)]
       [solution (find-solution board empty)])
  (display-move-path board solution))
