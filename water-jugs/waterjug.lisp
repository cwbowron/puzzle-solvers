;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CSE 440 - Section 3
;;; Project 4 - Waterjugs

;;; Christopher Bowron - bowronch@cse.msu.edu
;;; A26086081

;;; - Analysis -
;;; note : path length includes starting state - path cost is 1 less than
;;; length

;;;              - Depth First -
;;;        Path Length     Nodes Examined:
;;;  0        1              1
;;;  1       16             51
;;;  2       10             29
;;;  3        4              7
;;;  4       18             58
;;;  5       12             36
;;;  6        6             14 
;;;  7        2              2 
;;;  8       14             43 
;;;  9        8             21
;;; 10        3              4 

;;;            - Breadth First -
;;;        Path Length     Nodes Examined:
;;;  0        1              1 
;;;  1        6             53
;;;  2        8             82
;;;  3        2              3
;;;  4        4             29
;;;  5       10            101
;;;  6        4             33
;;;  7        2              2
;;;  8        8             74
;;;  9        6             57 
;;; 10        3              9

(defparameter *capacity* (cons 3 7))

(defvar *nodes* 0)

(defun prepend (a b)
  (append b a))

(defun postpend (a b)
  (append a b))

(defun goal-search (queue goal history expand appnd)
  (incf *nodes*)
  (let* ((state (pop queue))
	 (jugs (car state))
	 (path (cdr state))
	 (j1 (car jugs))
	 (j2 (cdr jugs)))
    
    ;;(format t "state : ~a~%" state)
    (cond
     ;; end of the line...
     ((null state) (return-from goal-search nil))

     ;; we are done
     ((= (+ j1 j2) goal) (return-from goal-search (nreverse (cons jugs path))))

     ;; expand node if not in history
     ((not (find jugs history :test #'equalp))
      (push jugs history)
      (push jugs path)
      ;;(format t "new path ~A~%" path)
      (let* ((expansion (funcall expand jugs path))
	     (newq (funcall appnd queue expansion)))
	;;(format t "expansion: ~a~%" expansion)
	(setq queue newq))))

    (goal-search queue goal history expand appnd)))
     

(defun jug-expand (state path)
  (let ((j1 (car state))
	(j2 (cdr state))
	(max1 (car *capacity*))
	(max2 (cdr *capacity*)))
    (let ((total (+ j1 j2)))
      (list
       (cons (cons j1 max2) path)	;; fill big jug
       (cons (cons max1 j2) path)	;; fill small jug
       (cons (cons j1 0) path)  	;; empty big jug
       (cons (cons 0 j2) path)		;; empty small jug
       (if (> total max2)               ;; pour litte into big
	   (cons (cons (- total max2) max2) path)
	 (cons (cons 0 total) path))
       (if (> total max1)		;; pour big onto little
	   (cons (cons max1 (- total max1)) path)
	 (cons (cons total 0) path))))))
	   
	   
(defun water-depth (goal)
  (setq *nodes* 0)
  (let ((path (goal-search (list (cons (cons 0 0) nil))
			   goal () 'jug-expand 'prepend)))
    (format t "PATH: ~A~%" path)
    (format t "LENGTH: ~A~%" (length path))
    (format t "NODES: ~a~%" *nodes*)))

(defun water-breadth (goal)
  (setq *nodes* 0)
  (let ((path (goal-search (list (cons (cons 0 0) nil))
			   goal () 'jug-expand 'postpend)))
    (format t "PATH: ~A~%" path)
    (format t "LENGTH: ~A~%" (length path))
    (format t "NODES: ~a~%" *nodes*)))
  

(defun do-all ()
  (dotimes (i 11)
	   (format t "~A - Depth ~%" i)
	   (water-depth i))

  (dotimes (i 11)
	   (format t "~A - Breadth~%" i)
	   (water-breadth i)))
  