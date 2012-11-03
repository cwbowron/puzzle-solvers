;;; SUDOKU solver and generator
;;; Christopher "Puzzle Man" Bowron <puzzleman@bowron.us>

(proclaim '(optimize (speed 3)))

(DEFCONSTANT SIZE-ROW 9)
(DEFCONSTANT SIZE-COL 9)

(defun solve-puzzle (puzzle &key debug randomize reverse)
  (dotimes (row SIZE-ROW)
    (dotimes (col SIZE-COL)
      (unless (get-number puzzle row col)
	(let ((possibilities
	       (get-possibilities puzzle row col)))
	  (when debug
	    (format t "~%~%row = ~A, col = ~A~%" row col)
	    (format t "possibilities:: ~A~%" possibilities)
	    (format t "~A~%" puzzle))
	  
	  (dolist (poss (cond
			   (reverse (nreverse possibilities))
			   (randomize (randomize-list possibilities))
			   (t possibilities)))
	    (let ((working-copy (copy-tree puzzle)))
	      (setf (nth col (nth row working-copy)) poss)
	      (let ((r (solve-puzzle working-copy
				     :debug debug
				     :randomize randomize
				     :reverse reverse)))
		(when r
		  (return-from solve-puzzle r))))))
	(return-from solve-puzzle nil))))
  puzzle)

(defvar *solve-puzzle-fast-counter* 0)

;;; optimize by solving for rows/cols/squares with the fewest
;;; possibilties first
(defun solve-puzzle-fast (puzzle &key debug randomize reverse)
  (incf *solve-puzzle-fast-counter*)
  (let ((ordering (get-ordering puzzle)))
    (dolist (item ordering)
      (let ((row (car (car item)))
	    (col (cdr (car item))))
	(unless (get-number puzzle row col)
	  (let ((possibilities (cdr item)))
	    (when debug
	      (format t "~%~%row = ~A, col = ~A~%" row col)
	      (format t "possibilities:: ~A~%" possibilities)
	      (pretty-print-puzzle puzzle))
	    (dolist (poss (cond
			   (reverse (nreverse possibilities))
			   (randomize (randomize-list possibilities))
			   (t possibilities)))
	      (let ((working-copy (copy-tree puzzle)))
		(setf (nth col (nth row working-copy)) poss)
		(let ((r (solve-puzzle-fast working-copy
				       :debug debug
				       :randomize randomize
				       :reverse reverse)))
		  (when r
		    (return-from solve-puzzle-fast r))))))
	  (return-from solve-puzzle-fast nil)))))
  puzzle)

;;; generate a solution by "solving" a blank grid using
;;; random permutation
;;; then randomly remove items that allow for the puzzle to still
;;; be solved to the original solution
;;; returns the puzzle and the solution
(defun generate-puzzle (&key debug min template)
  (let ((solution (solve-puzzle-fast *blank* :randomize t)))
    (values
     (if template
	 (apply-template template solution)
       (generate-puzzle-from-solution solution :debug debug :min min))
     solution)))

;;; from a given solution find a random puzzle with a unique solution
;;; having at least min numbers (if min is nil remove as many as possible)
(defun generate-puzzle-from-solution (solution &key min debug)
  (let ((puzzle (copy-tree solution)))
    (dolist (index (randomize-list (number-list 0 80)))
      (when (or (not min) (< min (count-numbers puzzle)))
	(let ((working-copy (copy-tree puzzle))
	      (row (floor (/ index SIZE-ROW)))
	      (col (mod index SIZE-COL)))
	  (setf (nth col (nth row working-copy)) nil)
	  (when (and
		 (equal solution (solve-puzzle-fast working-copy))
		 (equal solution
			(solve-puzzle-fast working-copy :reverse t)))
	    (when debug (pretty-print-puzzle working-copy))
	    (setq puzzle working-copy)))))
    puzzle))

;;; applies a template row to a solution row...
(defun apply-template-row (t-row s-row)
  (map 'list #'(lambda (t-elt s-elt)
		 (when t-elt s-elt)) t-row s-row))

;;; produce a puzzle from a template.  If the element is
;;; defined in template it will be taken from solution.  otherwise
;;; it will be nil.
(defun apply-template (template solution)
  (map 'list #'apply-template-row template solution))

;;; returns a list of numbers from min to max inclusive
(defun number-list (min max)
  (let ((numbers nil)
	(n (+ 1 (- max min))))
    (dotimes (i n)
      (setq numbers (cons (+ min i) numbers)))
    (nreverse numbers)))

;;; returns a list in random order
(defun randomize-list (some-list)
  (do ((randomized-list nil))
      ((null some-list) randomized-list)
    (let ((item-number (random (length some-list))))
      (push (elt some-list item-number)
            randomized-list)
      (setf some-list
            (remove (elt some-list item-number) some-list)))))

(defvar *all-possible-numbers-forward*
  (list 1 2 3 4 5 6 7 8 9))

(defvar *all-possible-numbers-backward*
  (list 9 8 7 6 5 4 3 2 1))

(defun possible-numbers (list)
  (set-difference *all-possible-numbers-forward* list))

(defun get-column (puzzle column)
  (map 'list #'(lambda (x) (nth column x)) puzzle))

(defun get-row (puzzle row)
  (nth row puzzle))

(defun get-number (puzzle r c)
  (nth c (nth r puzzle)))

;; gets the contents of the 3x3 square in which this (row,col) resides
(defun get-ninth (puzzle row col)
  (let ((r (floor (/ row 3)))
	(c (floor (/ col 3))))
    (let ((ninth nil))
      (dotimes (x 3)
	(dotimes (y 3)
	  (push
	   (get-number puzzle (+ (* r 3) y) (+ (* c 3) x))
	   ninth)))
      (nreverse ninth))))

;;; Count how many missing numbers there are in the list 
(defun count-missing (list)
  (count-if #'null list))

;;; count the numbers in a row or puzzle
(defun count-numbers (list)
  (cond ((numberp list) 1)
	((null list) 0)
	(t (reduce #'+ (map 'list #'count-numbers list)))))

(defun get-possibilities-row (puzzle row)
  (possible-numbers (get-row puzzle row)))

(defun get-possibilities-col (puzzle col)
  (possible-numbers (get-column puzzle col)))

(defun get-possibilities-square (puzzle row col)
  (possible-numbers (get-ninth puzzle row col)))

(defun get-possibilities (puzzle row col)
  (unless (get-number puzzle row col)
    (possible-numbers
     (union 
      (union (get-row puzzle row)
	     (get-column puzzle col))
      (get-ninth puzzle row col)))))


;;; count how many squares in row have n as a possilbility
(defun count-poss-row (puzzle n row)
  (let ((count 0))
    (dotimes (c 9)
      (let ((p (get-possibilities puzzle row c)))
	(when (find n p)
	  (incf count))))
    count))


(defun count-poss-col (puzzle n col)
  (let ((count 0))
    (dotimes (r 9)
      (let ((p (get-possibilities puzzle r col)))
	(when (find n p)
	  (incf count))))
    count))

(defun count-poss-square (puzzle n row col)
  (let ((count 0)
	(r (floor (/ row 3)))
	(c (floor (/ col 3))))
    (dotimes (y 3)
      (dotimes (x 3)
	(let ((ry (+ y (* 3 r)))
	      (cx (+ x (* 3 c))))
	  (let ((p (get-possibilities puzzle ry cx)))
	    (when (find n p)
	      (incf count))))))
    count))

(defun get-poss-cool (puzzle row col)
  (let ((p (get-possibilities puzzle row col)))
    (dolist (n p)
      (let ((r-count (count-poss-row puzzle n row))
	    (c-count (count-poss-col puzzle n col))
	    (s-count (count-poss-square puzzle n row col)))
	(when (= (min r-count c-count s-count) 1)
	  (return-from get-poss-cool (list n)))))
    p))

;;; doesn't work... trying to make a more logical possibilties function
(defun get-poss-crazy (puzzle row col)
  (let ((row-not (number-list 0 9))
	(col-not (number-list 0 9))
	(r (floor (/ row 3)))
	(c (floor (/ col 3))))
    (dotimes (x 3)
      (let ((rx (+ r x))
	    (cx (+ c x)))
	(unless (= rx row)
	  (setq row-not (intersection row-not (get-row puzzle rx))))
	(unless (= cx col)
	  (setq col-not (intersection col-not (get-column puzzle cx))))))
    (format t "ROW = ~A, COL = ~A~%" row col)
    (format t "ROW-NOT: ~A~%" row-not)
    (format t "COL-NOT: ~A~%" col-not)
    (let ((universe (number-list 0 9))
	  (poss (get-possibilities puzzle row col)))
      (when row-not
	(setq universe (intersection universe row-not)))
      (when col-not
	(setq universe (intersection universe col-not)))
      (let ((result (if (intersection universe poss)
			(intersection universe poss)
		      poss)))
	(format t "POSS: ~A~%" poss)
	(format t "RESULT: ~A~%" result)
	result))))

(defun get-ordering (puzzle)
  (let ((ordering nil))
    (dotimes (row 9)
      (dotimes (col 9)
	(unless (get-number puzzle row col)
	  (let ((values 
		 (get-possibilities puzzle row col)))
	    (push (cons (cons row col) values) ordering)))))
    (sort ordering #'< :key
	  #'(lambda (x) (length (cdr x))))))

(defun pretty-print-puzzle (puzzle)
  (let ((row-divider "+-------+-------+-------+~%"))
    (dotimes (row 9)
      (when (= 0 (mod row 3))
	(format t row-divider))
      (dotimes (col 9)
	(when (= 0 (mod col 3))
	  (format t "| "))
	(let ((n (get-number puzzle row col)))
	  (format t "~A " (if n n "_"))))
      (format t "|~%"))
    (format t row-divider)))

(defvar
 *puzzle*
 '((6   nil nil nil 4   nil nil nil nil)
   (nil 3   nil 8   nil nil 6   nil nil)
   (nil 9   2   nil 1   7   nil 4   3)
   (nil nil nil nil nil 2   4   nil nil)
   (nil 4   5   1   3   6   8   7   nil)
   (nil nil 6   4   nil nil nil nil nil)
   (3   7   nil 2   8   nil 9   6   nil)
   (nil nil 9   nil nil 1   nil 2   nil)
   (nil nil nil nil 7   nil nil nil 1)))

(defvar
 *evil*
 '((4 nil nil nil 5 6 nil nil 3)
   (nil nil nil nil 9 nil 5 nil nil)
   (nil nil nil 7 nil nil 6 nil nil)
   (nil nil 8 nil nil 4 nil 1 nil)
   (6 1 nil nil nil nil nil 9 7)
   (nil 5 nil 8 nil nil 2 nil nil)
   (nil nil 2 nil nil 7 nil nil nil)
   (nil nil 7 nil 8 nil nil nil nil)
   (1 nil nil 6 4 nil nil nil 9)))

(defvar
 *blank*
 '((nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil)
   (nil nil nil nil nil nil nil nil nil)))

(defvar 
  *super-hard*
  '((nil nil 1 2 nil nil nil 6 nil)
    (nil nil 9 nil nil 8 nil 4 nil)
    (nil 5 nil nil 4 nil 9 nil nil)
    (7 3 nil nil 8 nil nil nil nil)
    (nil nil 5 nil 3 nil 1 nil nil)
    (nil nil nil nil 6 nil nil 3 4)
    (nil nil 3 nil 2 nil nil 9 nil)
    (nil 2 nil 8 nil nil 5 nil nil)
    (nil 9 nil nil nil 1 4 nil nil)))

(defvar
  *blip*
  '((nil nil 6 nil 1 3 nil nil nil)
    (3 9 nil nil nil nil nil 1 nil)
    (2 nil 8 nil nil nil 4 nil nil)
    (nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil)
    (nil nil nil nil nil nil nil nil nil)))

(setq *ts* 
  '((nil 5 2 nil nil nil nil nil 8)
    (nil nil nil nil nil nil 9 nil nil)
    (8 nil nil 3 7 4 nil nil nil )
    (nil 8 9 nil nil nil nil 5 nil )
    (7 nil nil 6 9 3 nil nil 4)
    (nil nil nil nil nil nil 1 6 nil )
    (nil nil nil 2 5 6 nil nil 1)
    (nil nil 1 nil nil nil nil nil nil )
    (4 nil nil nil nil nil 3 2 nil )))


;; (solve-puzzle *puzzle*)
;; (solve-puzzle *evil*)

;;; Brian's puzzle
;; +-------+-------+-------+
;; | 6 _ _ | _ _ _ | _ 8 7 |
;; | _ 5 _ | _ _ _ | _ _ _ |
;; | _ 4 2 | 9 _ _ | _ _ _ |
;; +-------+-------+-------+
;; | _ _ 6 | _ _ _ | 1 _ _ |
;; | _ _ _ | 7 4 _ | _ _ _ |
;; | 3 _ _ | _ _ 8 | _ 5 _ |
;; +-------+-------+-------+
;; | _ 2 _ | _ _ _ | _ _ _ |
;; | _ _ _ | _ _ _ | 5 _ _ |
;; | _ _ _ | _ 3 4 | _ 1 6 |
;; +-------+-------+-------+
;;; SOLUTION:
;; +-------+-------+-------+
;; | 6 3 1 | 4 5 2 | 9 8 7 |
;; | 9 5 8 | 3 6 7 | 4 2 1 |
;; | 7 4 2 | 9 8 1 | 3 6 5 |
;; +-------+-------+-------+
;; | 4 8 6 | 5 9 3 | 1 7 2 |
;; | 2 1 5 | 7 4 6 | 8 9 3 |
;; | 3 7 9 | 1 2 8 | 6 5 4 |
;; +-------+-------+-------+
;; | 8 2 4 | 6 1 5 | 7 3 9 |
;; | 1 6 3 | 2 7 9 | 5 4 8 |
;; | 5 9 7 | 8 3 4 | 2 1 6 |
;; +-------+-------+-------+

;; ((6 NIL NIL NIL NIL NIL NIL 8 7)
;;  (NIL 5 NIL NIL NIL NIL NIL NIL NIL)
;;  (NIL 4 2 9 NIL NIL NIL NIL NIL)
;;  (NIL NIL 6 NIL NIL NIL 1 NIL NIL)
;;  (NIL NIL NIL 7 4 NIL NIL NIL NIL)
;;  (3 NIL NIL NIL NIL 8 NIL 5 NIL)
;;  (NIL 2 NIL NIL NIL NIL NIL NIL NIL)
;;  (NIL NIL NIL NIL NIL NIL 5 NIL NIL)
;;  (NIL NIL NIL NIL 3 4 NIL 1 6))
