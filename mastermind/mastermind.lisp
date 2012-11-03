;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Christopher Bowron - bowronch@cse.msu.edu
;;; CSE 440 - Artificial Intelligence

;;; final project - mastermind

;;; this program is pretty inefficient...
;;; specifically, it uses a lot of time generating poor guesses,
;;; but this way, the entire set of possible combinations does not
;;; have to reside in memory

;;; for best results, compile

;;; maximum observed attempts:      Possible Combinations

;;;  4 colors, 4 marbles : 6               256
;;; 11 colors, 4 marbles : 7           4194304
;;; 20 colors, 4 marbles : 15    1099511627776
;;; 25 colors, 4 marbles : 15 1125899906842624
;;; 11 colors, 5 marbles : 8          48828125
;;; 11 colors, 6 marbles : 7         362797056
;;;  4 colors, 8 marbles : 5              4096
;;;  6 colors, 8 marbles : 8            262144

;;; the computer will give up after this many attempts
(defparameter *MAX-GUESS* 40)
(defparameter *debug* nil)

;;; oooh... look at the pretty colors
;;; if you want more colors, just add them to this list
(defconstant *colors*
  '(white black red green yellow blue brown orange purple cyan magenta
	  indigo violet maroon crimson ecru tan pink teal grey
	  silver gold platinum rust charcoal))

(defstruct history-struct
  (marbles nil :type list)		; the marbles
  (colors  0   :type fixnum)		; right color, wrong spot
  (locations 0 :type fixnum))		; right color, right spot

(defun debug-format (&rest ops)
  (when *debug*
    (apply #'format ops)))

;; play a game of mastermind, with the computer as code breaker
(defun mm ()
  (let ((colors 0) (count 0) (marbles nil))
    (format t "Number of Colors (0-~a): " (length *colors*))
    (setq colors (read))
    (format t "Number of Marbles: ")
    (setq count (read))
    (format t "Possible combination: ~a~%" (expt count colors))
    (format t "Available Colors: ")
    (dolist (color (subseq *colors* 0 colors))
	    (format t "~a " color))
    (format t "~%")
    ;; inputs the marbles ahead of time
    ;; the program does not unfairly use this information
    ;; to make guesses
    (loop
     (format t "Marbles (e.g. (RED RED RED RED)): ")
     (setq marbles (read))
     ;; make sure we have a valid list of marbles
     (cond
      ((not (= (length marbles) count))
       (format t "Wrong number of marbles - try again~%"))
      ((not (let ((found t)
		  (color-list (subseq *colors* 0 colors)))
	      (dolist (item marbles)
		      (when (not (find item color-list))
			(setq found nil)))
	      found))
       (format t "Invalid color - try again~%"))
      (t (return t))))
    (mm-internal colors count marbles)))

;;; generate a guess based on a list of possible colors,
;;; colors that we know are in there, 
;;; locations that we know the color for,
;;; and locations that we know cannot be specific colors
(defun generate-guess (possible colors locations negatives)
  (let ((index-list nil) (index 0) (guess (copy-list locations)))

    (dolist (known locations)
	    (unless known
	      (push index index-list))
	    (incf index))
    
    (dolist (color colors)
	    (let ((attempts 0))
	      (loop
	       (setq index (random-elt index-list))
	       (unless (find color (nth index negatives))
		 (return t))
	       (incf attempts)
	       ;; if this configuration wont work,
	       ;; run generate-guess again
	       (when (> attempts 15)
		 (debug-format t "recursing~%")
		 (return-from generate-guess
			      (generate-guess possible colors
					      locations negatives)))))
	    (setq index-list (delete index index-list))
	    (setf (nth index guess) color))

    (dotimes (i (length guess))
	     (when (null (nth i guess))
	       (setf (nth i guess)
		     (loop
		      ;; make sure this color can actually go here
		      (let ((e (random-elt possible)))
			(unless (find e (nth i negatives))
			  (return e)))))))
    guess))

;;; handles duplicates correctly
(defun my-intersection (list1 list2)
  (let ((int nil) (list3 (copy-list list2))) ;; only copy it once
    (dolist (item list1)
	    (when (find item list3)
	      (setq list3 (delete item list3 :count 1))	;; destructive 
	      (push item int)))
    int))

;;; return t if guess and any item of history share more or less colors than 
;;; history item was correct on...
(defun color-test (guess history)
  (dolist (h-item history)
	  (let ((total (+ (history-struct-colors h-item)
			  (history-struct-locations h-item))))
	    (when (not (= (length
			   (my-intersection guess
					  (history-struct-marbles h-item)))
			  total))
	      (debug-format t "Too close: ~a - ~a~%" guess
		      (history-struct-marbles h-item))
	      (debug-format t "only worth : ~a~%" total)
	      (return-from color-test t))))
  nil)

;;; return t if guess and any item of history share more or less locations than
;;; history item was correct on... 
(defun location-test (guess history)
  (dolist (h-item history)
	  (let ((good (history-struct-locations h-item)))
	    (when
		(not (= (count t
			       (mapcar #'eq guess
				       (history-struct-marbles h-item)))
			good))
	      (debug-format t "Too close: ~a - ~a~%" guess
		      (history-struct-marbles h-item))
	      (debug-format t "only worth : ~a~%" good)
	      (return-from location-test t))))
  nil)

;;; return a random element from a list
(defun random-elt (list)
  (nth (random (length list)) list))

;;; return the a cons of the number of right colors, wrong spot
;;; and the number of right colors, right spot
(defun test-guess (guess actual)
  (let ((colors 0) (locations 0) (temp-list nil) (temp-guess nil))
    (dotimes (index (length guess))
	     (let ((guess-item (nth index guess))
		   (actual-item (nth index actual)))
	       (if (eq guess-item actual-item)
		   (incf locations)
		 (progn
		   (push actual-item temp-list)
		   (push guess-item temp-guess)))))
    (dolist (guess-item temp-guess)
	    (when (find guess-item temp-list)
	      (incf colors)
	      (setq temp-list (delete guess-item temp-list :count 1))))
    (cons colors locations)))

;; internal code-breaker function
(defun mm-internal (colors count marbles)
  (let ((possible (subseq *colors* 0 colors)) ; possible colors for our guesses
	(colors nil)			; colors we know are in it
	(locations nil)			; colors and locations we know
	(negatives nil)			; things we know dont go in spots
	(history nil)			; history of guesses and outcome
	(guess nil)			; a guess
	(guess-his nil)			; a guess as a history-struct
	(correctness nil)		; how well did we do (cons)
	(colors-count 0)		; how well we did colors
	(locations-count 0))		; how well we did locations
    
    (dotimes (i count)
	     (push nil locations)	; we dont know any marbles yet
	     (push nil negatives))	; we have no idea yet

    (dotimes (j *MAX-GUESS*)

      ;; print out some debugging info
      (format t "=============================~%")
      (format t "~:R guess~%" (1+ j))
      (debug-format t "Knowns : ~a~%" locations)
      (debug-format t "known colors: ~A~%" colors)
      (debug-format t "Possible Colors: ~a~%" possible)
      (debug-format t "negatives: ~A~%" negatives)
      ;;(debug-format t "history : ~A~%" history)
      
      ;; generate a guess and make sure we haven't tried it before
      (loop
       (setq guess (generate-guess possible colors locations negatives))
       ;; test feasibility of this guess loop if not feasible
       (cond
	;; if in history, then loop
	((find guess history :test #'equalp :key 'history-struct-marbles)
	 (debug-format t "Previously generated guess: ~a, looping~%" guess))

	;; if same colors as previous guess, loop
	((color-test guess history)
	 (debug-format t "impossible guess - colors, looping~%"))

	;; if too many exact marbles, loop
	((location-test guess history)
	 (debug-format t "impossible guess - locations, looping~%"))

	;; otherwise we are good to go
	(t (return t))))

      ;; turn guess into a history item
      (setq guess-his (make-history-struct :marbles guess))

      ;; calculate correctness
      (setq correctness (test-guess guess marbles))
      (setq colors-count (car correctness))
      (setq locations-count (cdr correctness))

      ;; print out this information
      (format t "this guess  : ~A~%" guess)
      (format t "actual marbles: ~A~%" marbles)
      (format t "right color, right spot : ~a~%" locations-count)
      (format t "right color, wrong spot : ~A~%" colors-count)
      (setf (history-struct-colors guess-his) colors-count)
      (setf (history-struct-locations guess-his) locations-count)

      ;; have we solved it?
      (when (= locations-count count)
	(format t "- Ha ha, I have broken your code -~%")
	(format t "- I am going to sell your secrets to cuba in")
	(format t " exchange for cigars -~%")
	(return-from mm-internal (values guess (1+ j))))
      
      ;; update our knowns based on this guess

      ;; we know these dont go here
      (when (= locations-count 0)
	(dotimes (index count)
		 (pushnew (nth index guess) (nth index negatives))))

      ;; we have failed miserably... these colors are not in there
      (when (= (+ locations-count colors-count) 0)
	(dolist (item guess)
		(setq possible (delete item possible))))

      ;; if we guess all the same color, we know how many of those color
      ;; are in the code, (obscure, but practical)
      (when (= (count (car guess) guess) count)
	(dotimes (i (- locations-count (count (car guess) locations)))
		 (push (car guess) colors)))
      
      ;; we have all the colors
      (when (= (+ locations-count colors-count) count)
	(dolist (known locations)
		(when known
		  (setq guess (delete known guess :count 1))))
	(setq colors guess))

      ;; compare this guess with items in history for inferences
      ;; may not actually be necessary with the more advanced guess testing
      (when (= (+ locations-count colors-count) (1- count))
      ;;(when (= locations-count (1- count))
	(dolist (h-item history)
		(let ((h-colors (history-struct-colors h-item))
		      (h-locations (history-struct-locations h-item))
		      (h-guess (history-struct-marbles h-item)))
		  (when (= (+ h-colors h-locations) (1- count))
		    (debug-format t "Hard core inferences about to be made~%")
		    (debug-format t "~A~%~A~%" guess-his h-item)
		    (cond
		     ((and (= h-locations (1- count))
			   (= locations-count (1- count)))
		      (debug-format t "making hard core inference~%")
		      (dotimes (index count)
			       (when (eql (nth index h-guess)
					  (nth index guess))
				 (unless (nth index locations)
				   (setq colors (delete
						 (nth index guess) colors
						 :count 1))
				   (setf (nth index locations)
					 (nth index guess))))))
		     (t ))))))
      
      (push guess-his history))
    
    (format t "- I have tried many times, but alas I am stumped -~%")))

;; computer creates code, player must try to break the code
(defun code-maker (colors count)
  (let* ((temp nil) (code nil) (attempts 0))
    (dotimes (i count)
	     (push nil temp))
    (setq code (generate-guess (subseq *colors* 0 colors) nil temp temp))
			      
    (format t "Available Colors: ")
    (dolist (color (subseq *colors* 0 colors))
	    (format t "~a " color))
    (format t "~%")
    (loop
     (incf attempts)
     (format t "=================~%")
     (format t "Your Guess: ")
     (let ((guess (read)))
       (let* ((good (test-guess guess code))
	      (colors (car good))
	      (locations (cdr good)))
	 (format t "Right Color, Right Spot : ~A~%" locations)
	 (format t "Right Color, Wrong Spot : ~a~%" colors)
	 (when (null guess)
	   (format t "- I see you give up~%")
	   (return-from code-maker (values nil (1- attempts))))
	 (when (= locations count)
	   (format t "- You have broken my code in ~a attempts~%" attempts)
	   (return-from code-maker (values code attempts))))))))
       
  