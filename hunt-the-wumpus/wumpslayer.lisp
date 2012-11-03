;; wumpslayer.lisp

;; Christopher Bowron - bowronch@cse.msu.edu
;; A26086081

;; CSE 440 - Section 3
;; Project 2
;; wumpus agent

;; How it works:
;;   turn around in the face of danger, otherwise use a
;;   basic flood fill algorithm (left, forward, right, backup)
;;   if we get gold retreat the same way we got here, just to be safe

;;   also track the wumpus, and kill him if it can be discovered where
;;   he is is for certain.

;;   i have added an instruction called backup that is treated as a forward,
;;   but will not save the next two moves, this allows for a turn around
;;   that is not saved onto our undo list

(defconstant *map-x* 40)
(defconstant *map-y* 40)
(defconstant *mid-x* (/ *map-x* 2))
(defconstant *mid-y* (/ *map-y* 2))
(defparameter *debug* nil)

(defun test (&optional (size 6))
  (run-wumpus :agents (list (reflex-agent 'S)) :display t
	      :max-steps (* size size 10)
	      :env (make-wumpus-world :agents (list (reflex-agent 'S))
				      :x-size size
				      :y-size size)))

;;(defun quiet ()
;;  (setq ext:*gc-verbose* nil))

(defun get-dx (direction)
  (case direction
	('east   1)
	('west  -1)
	(t       0)))

(defun get-dy (direction)
  (case direction
	('south -1)
	('north  1)
	(t       0)))
	 
(defun next-left (direction)
  (case direction
	('south 'east)
	('east  'north)
	('north 'west)
	('west  'south)))

(defun next-right (direction)
  (case direction
	('south 'west)
	('west  'north)
	('north 'east)
	('east  'south)))

;; optimize.. remove redundant moves
(defun optimize (list)
  (cond
   ((not (>= (length list) 2)) list)
   ((or 
     (equalp (subseq list 0 2) '((turn right) (turn left)))
     (equalp (subseq list 0 2) '((turn left) (turn right))))
    (debug-print "optimizing redundant turns ~a~%" list)
    (pop list)
    (pop list)
    list)
   ((not (>= (length list) 3)) list)
   ((equalp (subseq list 0 3) '((turn left) (turn left) (turn left)))
    (debug-print "optimizing 3 turns ~a~%" list)
    (pop list)
    (pop list)
    (pop list)
    (push '(turn right) list))
   ((equalp (subseq list 0 3) '((turn right) (turn right) (turn right)))
    (debug-print "optimizing 3 turns ~a~%" list)
    (pop list)
    (pop list)
    (pop list)
    (push '(turn left) list))
   (t list)))

(defun debug-print (&rest stuff)
  (push t stuff)
  (when *debug*
    (apply #'format stuff)))

;; percepts list:
;;    stench breeze glitter bump sound

;; action list
;; forward  (turn right) (turn left) climb shoot

(defun reflex-agent (name)
  (wump-slayer name))

(defun wump-slayer (name)
  (wumpus-agent 
   name 
   (let ((alive t)
	 (arrow t)
	 (gold nil)
	 (undo (list 'climb))
	 (immediate nil)
	 (long-range nil)
	 (starting t)
	 (x *mid-x*)
	 (y *mid-y*)
	 (nosave 0)
	 (facing 'east)
	 (mazemap (make-array (list *map-x* *map-y*) :initial-element nil))
	 (stench-map (make-array (list *map-x* *map-y*) :initial-element nil)))

     (defun wumpus? (x y)
       (let ((n (aref stench-map x (1+ y)))
	     (s (aref stench-map x (1- y)))
	     (w (aref stench-map (1- x) y))
	     (e (aref stench-map (1+ x) y))
	     (nw (aref mazemap (1- x) (1+ y)))
	     (ne (aref mazemap (1+ x) (1+ y)))
	     (sw (aref mazemap (1- x) (1- y)))
	     (se (aref mazemap (1+ x) (1- y))))
	 (cond
	  ((and n s) t)
	  ((and w e) t)
	  ((and n e ne) t)
	  ((and n w nw) t)
	  ((and s e se) t)
	  ((and s w sw) t)
	  (t nil))))

     (defun wumpus-find (x y)
       (cond
	((wumpus? x (1+ y)) 'north)
	((wumpus? x (1- y)) 'south)
	((wumpus? (1+ x) y) 'east)
	((wumpus? (1- x) y) 'west)
	(t nil)))

     (defun face (dir)
       (case facing
	     ('north
	      (case dir
		    ('north nil)
		    ('west  (list '(turn left)))
		    ('south (list '(turn left) '(turn left)))
		    ('east  (list '(turn right)))))
	     ('west  
	       (case dir
		    ('north (list '(turn right)))
		    ('west  nil)
		    ('south (list '(turn left)))
		    ('east  (list '(turn left) '(turn left)))))
	     ('south 
	      (case dir
		    ('north (list '(turn left) '(turn left)))
		    ('west  (list '(turn right)))
		    ('south nil)
		    ('east  (list '(turn left)))))
	     ('east 
	      (case dir
		    ('north (list '(turn left)))
		    ('west  (list '(turn left) '(turn left)))
		    ('south (list '(turn right)))
		    ('east  nil)))))
     
     (defun do-stuff (stuff)
       (debug-print "do-stuff ~a~%" stuff)
       ;; update state
       ;; once we have gold dont save our moves -- we are going back
       (when (> nosave 0)
	 (setq nosave (- nosave 1)))
       (cond

	((equalp stuff 'shoot)
	 (setq arrow nil))

	((equalp stuff 'forward)
	 (setq x (+ x (get-dx facing)))
	 (setq y (+ y (get-dy facing)))
	 (unless (or gold (> nosave 0))
	   (push 'forward undo)))

	((equalp stuff '(turn right))
	 (setq facing (next-right facing))
	 (unless (or gold (> nosave 0))
	   (push '(turn left) undo)))

	((equalp stuff '(turn left))
	 (setq facing (next-left facing))
	 (unless (or gold (> nosave 0))
	   (push '(turn right) undo))))

       ;; figure out what what stuff we should do
       (cond
	;; if we have more than one thing,
	;; store it and do the first
	((and
	  (listp stuff)
	  (not (equal (car stuff) 'turn)))
	 (setq immediate (cdr stuff))
	 (do-stuff (car stuff)))

	((equalp stuff 'backup)
	 (setq nosave 3)
	 (loop
	  (let ((last (pop undo)))
	    (debug-print "Inside of backup : action ~a~%" last)
	    (when (null undo)
	      (return 'forward))
	    (when (equalp last 'forward)
	      (setq x (+ x (get-dx facing)))
	      (setq y (+ y (get-dy facing)))
	      (return 'forward)))))

	(t stuff)))

     ;; make some long range plans
     (defun plan-stuff (stuff)
       (setq long-range (nconc stuff long-range)))

     ;; the true agent begins here
     ;; the wumpslayer that can be told is not the true wumpslayer
     #'(lambda (p)
	 (let ((start starting)
	       (visited (aref mazemap x y)))

	   (setf (aref mazemap x y) t)
	   
	   ;; we will know if we are at the very beginning..
	   (setq starting nil)

	   (debug-print "==================================================~%")
	   (when (wumpus-percept-breeze p)
	     (debug-print "Close that $@$! door~%"))
	   
	   (when (wumpus-percept-bump p)
	     (debug-print "!@#$#!@%#!%!@# My head hurts~%")
	     (case facing
		   ((or 'west 'east)
		    (dotimes (ty *map-y*)
			     (setf (aref mazemap x ty) t)))
		   ((or 'south 'north)
		    (dotimes (tx *map-x*)
			     (setf (aref mazemap tx y) t)))))
	   
	   (when (wumpus-percept-stench p)
	     (debug-print "Pheewww!!!! What smells~%")
	     (setf (aref stench-map x y) t)
	     (debug-print "The Wumpus was found : ~a~%" (wumpus-find x y)))
	   
	   (when (wumpus-percept-glitter p)
	     (debug-print "Oooohhh...  Shiny~%"))

	   (when (wumpus-percept-sound p)
	     (debug-print "Die Wumpus Scum~%")
	     (setq alive nil))

	   (when gold
	     (debug-print "Ha Ha I have gold and you don't~%"))

	   (unless (> nosave 0) 
	     (setq long-range (optimize long-range))
	     (setq immediate (optimize immediate)))
	   
	   (setq undo (optimize undo))
	   
	   (debug-print "Arrow : ~a, Alive : ~a, Gold : ~a~%" arrow alive gold)
	   (debug-print "Located at ~a, ~a : Facing ~a~%"
			(- x *mid-x*)
			(- y *mid-y*)
			facing)
	   (debug-print "Have we been here ? : ~a~%" visited)
	   (debug-print "Long Range : ~A~%" long-range)
	   (debug-print "Immediate : ~A~%" immediate)
	   (debug-print "Undo List: ~a~%" undo)
	   
	   ;; now figure out our action
	   (cond
	    ;; if we remember we want to do something, do it
	    (immediate
	     (do-stuff (pop immediate)))
	    
	    ;; if we have gold undo our moves
	    (gold
	     (cond
	      ((null undo)
	       (format t "Houston, we have a problem~%")
	       (random-element '((turn left) (turn right) forward climb)))
	      (t (do-stuff (pop undo)))))

	    ;; If we see gold, grab and turn around
	    ((wumpus-percept-glitter p)
	     (setq gold t)
	     (if start
		 (do-stuff '(grab climb))
	       (do-stuff '(grab (turn left) (turn left)))))
	    
	    ;; if we have breeze
	    ;; turn around or climb out
	    ((wumpus-percept-breeze p)
	     (if start
		 (do-stuff 'climb)
	       (do-stuff '((turn left) (turn left) backup
			   (turn left) (turn left)))))

	    ;; shoot at wumpus if we find him
	    ;; turn around or leave if we dont find him
	    ((and (wumpus-percept-stench p) alive)
	     (cond
	      ((and (wumpus-find x y) arrow)
	       (let ((inst 
		      (let* ((wdir (wumpus-find x y))
			     (restore facing)
			     (facewump (face wdir))
			     (stuff (append facewump
					    '(shoot))))
			
			(setq facing wdir)
			(setq stuff (append stuff (face restore)))
			(setq facing restore)
			stuff)))
		 
		 (plan-stuff
		  (list '(turn left)
			'forward
			'(turn right)
			'forward
			'(turn right)
			'forward
			'(turn right)
			'backup
			'(turn left)
			'(turn left)))
		 
		 ;; 2 because do-stuff will be called twice,
		 ;; once for initial list,
		 ;; once for first item,
		 (setq nosave (+ nosave (length inst) 2))
		 (do-stuff inst)))

	      ;; if we are at the start, might as well try to kill it 
	      ((and start arrow) (do-stuff 'shoot))

	      ;; climb out and live to explore another day
	      (start (do-stuff 'climb))
	      
	      ;; we are at the beginning, but we shot first move
	      ;; and didnt kill the beast
	      ((and (= x *mid-x*)
		    (= y *mid-y*))
	       (do-stuff 'climb))

	      ;; turn around and exit stage left
	      (t (do-stuff '((turn left) (turn left) backup
			     (turn left) (turn left))))))
	    
	    ;; if we have visited get our next plan
	    ((and visited long-range)
	     (do-stuff
	      (loop
	       (let ((action (pop long-range)))
		 ;; do the first long range plan,
		 ;; unless we have already visited that square
		 (unless (and (equal action 'forward)
			      (aref mazemap
				   (+ x (get-dx facing))
				   (+ y (get-dy facing))))
		   (return action))))))

	    ;; hhmmmm... what happened?
	    ((and visited
		  (not (wumpus-percept-sound p))
		  ;;(not (wumpus-percept-stench p))
		  )
	     (format t "something is wrong~%")
	     (random-element '((turn left) (turn right) forward climb)))

	    ;; when we hit a wall, restore our previous values for
	    ;; x and y and then continue on our way into our long
	    ;; range plans...
	    ((wumpus-percept-bump p)
	     ;; remove the last forward
	     (pop undo)
	     ;; restore coordinates
	     (setq x (- x (get-dx facing)))
	     (setq y (- y (get-dy facing)))
	     
	     (do-stuff
	      (loop
	       (let ((action (pop long-range)))
		 ;; do the first long range plan,
		 ;; unless we have already visited that square
		 (unless (and (equal action 'forward)
			      (aref mazemap
				   (+ x (get-dx facing))
				   (+ y (get-dy facing))))
		   (return action))))))

	    ;; default: make plans to visit our neighbors and say hi
	    (t
	     (if start
		 (plan-stuff
		  (list '(turn left)
			'forward
			'(turn right)
			'forward
			'(turn right)
			'forward
			'(turn right)
			'climb))
	       (plan-stuff
		(list '(turn left)
		      'forward
		      '(turn right)
		      'forward
		      '(turn right)
		      'forward
		      '(turn right)
		      'backup
		      '(turn left)
		      '(turn left))))
	       (do-stuff (pop long-range)))))))))

