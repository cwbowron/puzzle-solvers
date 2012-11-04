#lang scheme

(define DOOR-COUNT 3)

(define (car? n lst) (eq? (list-ref lst n) 'car))

(define (get-door-layout n)
  (let loop ([n n] [doors (list)])
    (cond
      [(= 0 n) doors]
      [(or (member 'car doors)
           (not (= 0 (random n))))
       (loop (sub1 n) (cons 'goat doors))]
      [else 
       (loop (sub1 n) (cons 'car doors))])))

(define (get-random-door door-count criteria-function)
  (let ([door (random door-count)])
    (if (criteria-function door) 
        door 
        (get-random-door door-count criteria-function))))

(define (get-revealed-door doors contestant-selection)
  (get-random-door (length doors) 
                   (lambda (n)
                     (and (not (= n contestant-selection))
                          (eq? (list-ref doors n) 'goat)))))

(define (get-switch-door door-count unavailable-doors)
  (get-random-door door-count (lambda (n) (not (member n unavailable-doors)))))

(define (make-a-deal n (show-debug-info #f))
  (let ([success-count-stick 0]
        [success-count-switch 0])
    (for ((trial (in-range n)))
      (let ([doors (get-door-layout DOOR-COUNT)]
            [selected-door (random DOOR-COUNT)])
        (let ([revealed-door (get-revealed-door doors selected-door)])
          (let ([selected-door-switch (get-switch-door DOOR-COUNT (list selected-door revealed-door))])
            (when show-debug-info
              (printf "DOORS: ~A~%" doors)
              (printf "SELECTED DOOR: ~A~%" selected-door)
              (printf "REVEALED DOOR: ~A~%" revealed-door)
              (printf "SWITCH DOOR: ~A~%" selected-door-switch))
            (cond [(car? selected-door doors)
                   (set! success-count-stick (add1 success-count-stick))]
                  [(car? selected-door-switch doors)
                   (set! success-count-switch (add1 success-count-switch))])))))
    (printf "Success Rate of Sticking:  ~A%~%" (exact->inexact (* 100 (/ success-count-stick n))))
    (printf "Success Rate of Switching: ~A%~%" (exact->inexact (* 100 (/ success-count-switch n))))
    (if (> success-count-switch success-count-stick)
        (printf "Better off Switching~%")
        (printf "Better off Sticking~%"))))

(make-a-deal 100000)
