#lang scheme/gui

[require scheme/vector]

(require "sudoku.ss")
 
(define sudoku-frame%
  (class frame% 
    
    (define (new-text-field-row p)
      (let ([strip (new horizontal-panel% [parent p])])
        (build-vector 
         (grid-size)
         (lambda (n)                      
           (when (and (not (zero? n)) (= 0 (modulo n (grid-size-root))))
             (new vertical-panel% [parent strip] [min-width 5]))
           (new text-field%
                [parent strip]
                [min-width 20]
                [vert-margin 0]
                [horiz-margin 0]
                [label #f]
                [init-value " "])))))
   
    (super-new [parent #f]
               [label "Sudoku"])
    
    (define text-field-grid
      (build-vector 
       (grid-size)
       (lambda (m)
         (when (and (not (zero? m)) (= 0 (modulo m (grid-size-root))))
           (new horizontal-panel% [parent this] [min-height 5]))         
         (new-text-field-row this))))
        
    (new button%
         [label "Solve"]
         [parent this]
         [callback
          (lambda (button event)
            (solve-2 (send this generate-solver-grid)
                     #:update
                     (lambda (g)
                       (send this set-cells g)
                       (send this refresh)
                       (sleep .1)
                       )))])
    
    (define/public (generate-solver-grid)
      (vector-map
       (lambda (v0)
         (vector-map
          (lambda (text-field)
            (string->number (send text-field get-value)))
          v0))
       text-field-grid))
    
    (define/public (set-cells grid)
      (for* ((m (in-range (grid-size)))
             (n (in-range (grid-size))))
        (let ([field [grid-ref text-field-grid m n]])
          (send field set-value
                (if (grid-ref grid m n)
                    (number->string (grid-ref grid m n))
                    " ")))))))
        

;;(define (visibly-solve grid)
;;  (let ([f (new sudoku-frame%)])
;;    (send f show #t)
;;    (send f set-cells grid)
;;    (yield)
;;    (solve-2 grid
;;             #:update
;;             (lambda (g)
;;               (send f set-cells g)
;;               (send f refresh)
;;               (sleep .1)))))

;;;;(visibly-solve SAMPLE-GRID)
