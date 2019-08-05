;Assignment 4

;David Lane
;004638553
;Korf/Chen

;Description: The following set of constants and functions are used together to implement Iterative Depth A* search, to find an optimal
;solution to the 8 tile puzzle 

(defvar manhattan8 '( 		(1 0 1 2 1 2 3 2 3)
							(2 1 0 3 2 1 4 3 2)
							(1 2 3 0 1 2 1 2 3)
							(2 1 2 1 0 1 2 1 2)
							(3 2 1 2 1 0 3 2 1)
							(2 3 4 1 2 3 0 1 2)		;Manhattan distance from tile to goal position
							(3 2 3 2 1 2 1 0 1)		;where y is tile number and x is position  
							(4 3 2 3 2 1 2 1 0))) ;as suggested by problem spec

;A set of functions for extracting the manhattan distance of a given state from the table

(defun tileatindex (pos state) ;takes a position and a state and returns the tile at that position in the state
								;can also be used to search a row of the manhattan8 table for the position value
	(cond 	((= pos 0) (car state))
			(t (tileatindex (- pos 1) (cdr state)))))

(defun manvalue (int index table) ;returns the manhattan value for one tile in the state
	(cond 	((= int 1) (tileatindex index (car table)))
			(t (manvalue (- int 1) index (cdr table)))))

(defun manhattan (state total index) ;sums the manhattan value for all tiles of a state
	(cond 	((null state) total)
			((= (car state) 0) (manhattan (cdr state) total (+ 1 index)))
			(t (manhattan (cdr state) (+ total (manvalue (car state) index manhattan8)) (+ 1 index)))))



(defvar validmoves '( 		(3 1)
							(4 2 0)
							(5 1)
							(6 4 0)
							(7 5 3 1)
							(8 4 2)
							(7 3)
							(8 6 4)	;the set of valid moves
							(7 5))) ;For each zero position (y axis)

;A set of functions to return the valid moves for zero given a state

(defun findtile (state tile pos) ;returns the position of tile in a state  
	(cond 	((= (car state) tile) pos)
			(t (findtile (cdr state) tile (+ pos 1)))))

(defun validmovesof (zeropos moves)  ;takes a position of zero and nil and returns the possible moves for that position
										;first argument will be (findtile state 0 0) (the position of zero)
	(cond 	((= zeropos 0) (car moves))
			(t (validmovesof (- zeropos 1) (cdr moves))))) 



;A set of functions to switch the zero tile with a tile at a given valid position

(defun remover (int lst) ;the standard common lisp "remove" function written by me, removes all instances of int from list
	(cond 	((null lst) lst)
			((= int (car lst)) (remover int (cdr lst)))
			(t (cons (car lst) (remover int (cdr lst))))))

(defun insert (int lst pos) ;returns a new list with int inserted at index pos in lst
	(cond 	((= pos 0) (cons int lst))
			(t (cons (car lst) (insert int (cdr lst) (- pos 1) ) ))))
 

(defun switcher (state newzeropos) ;switches the zero tile with the tile at newzeropos in state
	(let 	((x (findtile state 0 0)) ;x is the position of 0
			(y (tileatindex newzeropos state))) ;y is the tile at newzeropos
			(cond 	((< newzeropos x) (insert y (insert 0 (remover y (remover 0 state)) newzeropos) x )) ;conditional to make sure we insert the 
																									;the tile that needs to come first first
					(t (insert 0 (insert y (remover y (remover 0 state)) x ) newzeropos))))) ;remove 0 and numbered tile, add them both back in
																								;proper position



;The core functions of the Iterative Deepening A* search


;A function to determine if a state is the goal state

(defun goalstate (state zerotoeight) ;NOTE always call with 0 as argument zerotoeight initially
	(cond 	((null state) t)
			((= (car state) zerotoeight) (goalstate (cdr state) (+ zerotoeight 1))) ;iterate through a state checking if tile matches position
			(t nil)))
						
;A pair of functions which help to generate a final return value for helper,
;returns either the lowest found value of f in that layer of search, or the solution string

(defun lowerof (int1 int2) ;returns the lesser of int1 and int2
	(cond 	((> int1 int2) int2)
			(t int1)))

(defun neednewdepth (lst lowest) ;Note: lowest should be passed 32, one more than the max number of moves to optimally solve 8-puzzle
	(cond 	((null lst) lowest)
			((atom (car lst)) (neednewdepth (cdr lst) (lowerof (car lst) lowest))) ;if lst contains only atoms, return the lowest atom
			(t (car lst)))) ;if lst contains a string, return the string


;Core functions of the Iterative Deepening A* search

(defun gennewstates (state moveset moves endstates) ;takes in a state, the moves required to reach it, possible moves, and nil
													;returns a new list of ((state) (moveset))s to be evaluated
	(cond 	((null moves) endstates)
			(t (gennewstates 	state 
								moveset 
								(cdr moves) 
								(cons (list (switcher state (car moves)) (cons (tileatindex (car moves) state) moveset)) endstates)))))
									;generate a new state with the zero and number tile switched, and the tile that was moved added to its moveset
									;NOTE: add tile moved to the front of the moveset to make logic to avoid searching parent node constant time vs. linear 

					;NOTE: Form of Remaining ( ((state) (movedtiles)) )					
(defun helper (remaining depth maxf)	;generates all possible states at a certain depth of search f that dont have a , 
										;remaining is remaining states to be searched, depth is current depth and maxf is the maximum depth	
	(cond	((null remaining) nil)		;if remaining is empty, return nil
			((>  (+ depth (manhattan (car (car remaining)) 0 0)) maxf) (cons (+ depth (manhattan (car (car remaining)) 0 0)) (helper (cdr remaining) depth maxf)))
										;if f exceeds max f, note depth and call helper on the next state in remaining (prune)
			((goalstate (car (car remaining)) 0) (list (car (cdr (car remaining)))))	;when reaching a goal state return that goal state
			(t (cons 	(neednewdepth 	(helper 	;call recursively on new states, cleaning up with neednewdepth, so that each layer returns only an optimal
													;new depth or solution, thus keeping space complexity down
										(gennewstates 	(car (car remaining))
														(car (cdr (car remaining)))
														(remover (findtile (switcher (car (car remaining)) (findtile (car (car remaining)) (car (car (cdr (car remaining)))) 0) ) 0 0)
																 (validmovesof (findtile (car (car remaining)) 0 0) validmoves))
																 ;Logic for avoiding searching parent node
																 ;find the position of the first tile in the moveset (most recent move)
																 ;call switcher to switch zero back to that position
																 ;check that new state to find where 0 is
																 ;that was the position of 0 in the previous state
																 ;eliminate that position from validmoves
																 ;NOTE: solution string is backwards to make this slightly quicker, we reverse at the end
														nil)
										(+ depth 1) ;increment depth as we search a new layer
										maxf)
							32)
						(helper (cdr remaining) depth maxf ) ))))	

(defun newdepth (state maxdepth) 
	(let 	((x (car (helper state 0 maxdepth))))	 	
		(cond	((atom x) (cond ((>= x 32) nil)		;if helper returns a number, if that number is 32 or more, we have exceeded max moves and the initial state is bad
								(t (newdepth state x ) ))) ;otherwise call again at the new lowest f value depth
				(t x)))) 	;if helper returns a string, it is the solution string

(defun reverser (lst new) ;reverses the elements of a list
	(cond 	((null lst) new)
			(t (reverser (cdr lst) (cons (car lst) new)))))

(defun ida* (startstate) ;takes startstate and puts it into form to be handled by newdepth/helper
	(let ((x (newdepth  (list (list startstate '(0))) (manhattan startstate 0 0)))) ;0 added as first move in moveset for initial state so that
																					;calling car on the set would not be invalid
		(cond 	((goalstate startstate 0) nil) ;if startstate is good, no moves required
				((null x) 'fail)				;if the search returned nothing, instance not solvable
				(t (reverser (remover 0 x) nil)))))	;otherwise clean up the solution string and return

;NOTE: My time for solving (0 2 1 3 4 5 6 7 8) FAIL was 4 minutes on a 2.4 GHz processor



;TEST ZONE

;(write (switcher '(1 2 3 4 5 6 0 7 8) 2))	
;(write (reverser '(1 2 3 4 5 3 4 2 3) nil))
;(write nil)
;(write (IDA* '(5 7 4 2 3 0 8 1 6) ))	
;(time (write (IDAstar '(0 2 1 3 4 5 6 7 8) )))

;(time (write (IDAstar '(1 4 7 6 5 8 0 3 2) )))

;(write (switcher '(1 0 2 3 4 5 6 7 8) 0))	
;(write (remover 0 '(1 2 3 0 4 5 0)))
;(write (manhattan '(3 1 5 2 0 8 6 4 7) 0 0))	
;(write (manhattan '(1 0 2 3 4 5 6 7 8) 0 0))
;(write (neednewdepth '(3 1 5 2 0 8 6 4 7 (5)) 32))

;(write (ida* '(4 3 2 1 0 5 6 7 8)) )
;(write (ida* '(0 1 2 3 4 5 6 7 8)) )
;(time (write (ida* '(0 2 1 3 4 5 6 7 8)) ))
;(time (write (ida* '(8 0 6 5 4 7 2 3 1)) )) ;longest possible solution

(print '(4 8 6 5 1 6 3 7 6 3 5 1 3 6 7 4 8 5 4 7 6 3)) 
(write(ida* '(5 6 2 1 8 4 7 3 0)))

(print '(7 1 5 2 1 3 8 5 2 1 3 7 6 8 5 2 1 3 4 6 7 4 3)) (write (ida* '(5 1 8 2 7 3 4 0 6)))

(print (equal '(1 2 4 7 8 6 5 3 2 4 6 8 7 6 3 5 8 7 6 3 4 1) (ida* '(0 1 3 4 2 5 7 8 6))))

(print (equal '(7 6 3 2 6 3 8 4 5 7 3 6 1 3 6 5 4 8 5 4 7 6 3) (ida* '(1 2 3 0 7 6 5 4 8))))

(print (equal '(4 1 2 6 3 2 1 4 6 3 2 1 3 5 7 6 4 3 1 2 5 4 3) (ida* '(4 1 3 0 2 6 7 5 8))))

(print (equal '(3 2 1 3 4 6 8 4 6 5 2 1 3 6 4 7 6 3) (ida* '(2 3 5 1 0 4 7 8 6))))

(print (equal '(2 4 5 6 3 5 4 2 6 3 8 7 3 6 1 3 6 4 5 8 7 6 3) (ida* '(1 0 2 7 5 4 8 6 3))))

(print (equal '(4 3 8 5 6 4 3 8 7 2 1 3 4 6 8 7 5 8 7 4 3) (ida* '(1 2 7 0 4 3 6 5 8))))

(print (or (equal "FAIL" (ida* '(3 6 4 0 1 2 8 7 5))) (equal 'FAIL (ida* '(3 6 4 0 1 2 8 7 5)))))




;-------------------------------------------------------





;Scraps

#|(defun DFID (startstate remaining maxdepth)		;returns a moveset which is a list with a number of moves and a list of moves
	(cond	;((= depth 32) 'fail)
			((null remaining) nil)
			((goalstate (car (cdr (car remaining))) 0) moveset)
			((= (car (car remaining)) maxdepth) 
				(DFID 	startstate 
						(cdr remaining)
						maxdepth))			
			(t (DFID 	startstate 
						(append (gennewstates (car (cdr (car remaining))) (car (cdr (cdr remaining))) (validmovesof (findzero (car (cdr (car remaining))) 0) validmoves) nil (car (car remaining)) ) (cdr remaining) )
						maxdepth))))
|#
#|(defun gennewstates (startstate moveset moves endstates) ;takes in a state and returns the moves
	(cond 	((null moves) endstates)
			(t (gennewstates startstate moveset (cdr moves) (cons (list (switcher startstate (car moves)) (append (list (tileatindex (car moves) startstate)) moveset)) endstates)))))
	;'((0 1 2 3 4 5 6 7 8))) |#



#|(defun helper (remaining int)				;Auxiliary function for DFID (does constrained DFS)
	(cond	((null remaining) nil)
			((= int 0) nil )			;Two base cases, when depth to be searched is 0 return nil or
			((goalstate (car (car remaining)) 0) (car (cdr (car remaining))))	;when reaching the end of a tree or branch, return nil
			;((atom (car lst)) (cons (car lst) (helper (cdr lst) int) ))	;if the first item in the list is an atom
																		;append it and look for branches
			(t (append (helper (gennewstates 	(car (car remaining)) 
												(car (cdr (car remaining)))
												;(write 
													(remove (findtile (switcher (car (car remaining)) (findtile (car (car remaining)) (car (car (cdr (car remaining)))) 0) ) 0 0) 
													(validmovesof (findtile (car (car remaining)) 0 0) validmoves)
													)  
													;)
												nil)
								(- int 1)) 
						(helper (cdr remaining) int ) ))))	;do depth first search,
																				;unless youre at the max depth

(defun DFID (state maxdepth)			;Depth First Iterative Deepening
	(cond	((<= maxdepth 0) nil)	;base case, when depth to be searched is 0 return nil
			(t (helper state maxdepth) ))) 	;shell function to append different depths
																	;shallower calls first


(defun IDAstar (startstate maxdepth); maxdepth)
	(let ((x (DFID  (list (list startstate '(0))) maxdepth)))
		(cond 	((goalstate startstate 0) nil)
				((null x) 'fail)
				(t (remove 0 x)))))
		
|#


