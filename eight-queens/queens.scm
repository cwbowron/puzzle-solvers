;;; 8 queens solution, converted from Tiger
(define (queens)
  (let* ((N 8)
	 (row (make-vector N 0))
	 (col (make-vector N 0))
	 (diag1 (make-vector (+ N N -1) 0))
	 (diag2 (make-vector (+ N N -1) 0)))
    (letrec ((print-board
	      (lambda ()
		(do ((i 0 (+ i 1)))
		    ((>= i 8))
		  (do ((j 0 (+ j 1)))
		      ((>= j 8))
		    (display (if (= (vector-ref col i) j)
				 " 0"
				 " .")))
		  (newline))
		(newline)))
	     (try
	      (lambda (c)
		(if (= c N)
		    (print-board)
		    (do ((r 0 (+ r 1)))
			((>= r N))
		      (let ((r+7-c (- (+ r 7) c))
			    (r+c (+ r c)))
			(when (and (= (vector-ref row r) 0)
				   (= (vector-ref diag1 r+c) 0)
				   (= (vector-ref diag2 r+7-c) 0))
			      (vector-set! row r 1)
			      (vector-set! diag1 r+c 1)
			      (vector-set! diag2 r+7-c 1)
			      (vector-set! col c r)
			      (try (+ c 1))
			      (vector-set! row r 0)
			      (vector-set! diag1 r+c 0)
			      (vector-set! diag2 r+7-c 0))))))))
      (try 0))))
(queens)