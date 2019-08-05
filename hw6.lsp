;Assignment 6

;David Lane
;004638553
;Korf/Chen

;Description: A solution to the N-Queens puzzle using DFS with Backtracking.
;Solves the puzzle for up to N = 26/27 Queens in reasonable time



(defun QUEENS (n)
	(cond 	((= n 1) '(1))
			((< n 4) nil)	;eliminates impossible early cases quickly, cuz why not?
			(t (nqueener nil (genavailablesquares nil n) 0 n nil)))) ;Top level function prepares for recursion in nqueener


(defun nqueener (board availablesquares queenx maxqueens solution)  ;board = existing board of queens
																	;availablesquares = squares in the next column on the board where queen could be placed
																	;queenx = the xth queen, currently being placed
																	;maxqueens = n in the puzzle definition
																	;solution = nil, until a valid n-queen board is found
	(cond 	((not (null solution)) solution) ;base case: if we've found a solution, return that solution
			((= queenx maxqueens) board)	;if all of the queens are placed, return the board (as solution)
			((null availablesquares) nil)	;if there are no available squares at this iteration, backtrack
			(t (nqueener board (cdr availablesquares) queenx maxqueens ;recursive call, on previous board, with solution being a potential next board (new available squares and one more queen)
						(nqueener (cons (car availablesquares) board) (genavailablesquares (cons (car availablesquares) board) maxqueens) (+ queenx 1) maxqueens solution)))))


(defun genavailablesquares (board maxqueens) ;given a board, and n from the puzzle, generates all y coords (rows) where a queen could be placed in the next column on the board
	(cond 	((= 0 maxqueens) nil)	;once weve checked n rows, done	
			((canbeplaced board maxqueens 1) (cons maxqueens (genavailablesquares board (- maxqueens 1)))) ;if the queen could be placed at this row, add to list of available rows
			(t (genavailablesquares board (- maxqueens 1))))) 	;otherwise, omit this row, not available


(defun canbeplaced (board coordy columnsaway) ;checks whether a queen could be placed at a given row on a given board, columns away is to check diagonal interference
	(cond 	((null board) t) 	;if the board is empty, queen can always be placed
			((= coordy (car board)) nil)	;if there is already a queen in this row, return false
			;implements diagonal interference check
			((= coordy (+ (car board) columnsaway)) nil) 	
			((= coordy (- (car board) columnsaway)) nil)
			(t (canbeplaced (cdr board) coordy (+ columnsaway 1))))) ;check interference in next column, incrementing columnsaway to check next diagonals


;TESTING

;(write (QUEENS 25))
;(write (genavailablesquares '(2 4 4) 4))
;(write (canbeplaced '(3 4 5 1) 2 1))




;Scraps

#|(defun nqueener (board coordy queenx maxqueens)
	(cond 	((= queenx maxqueens) board)
			(t (let ((x (placequeen board coordy maxqueens))) ;try to place queen
					(cond 	((null x) (cond		((null board) board)
												(t (nqueener (cdr board) (+ 1 (car board)) (- queenx 1) maxqueens)))) ;if queen cant be placed with this board, backtrack		
							(t (nqueener x 1 (+ queenx 1) maxqueens)))))))|#


#|(defun placequeen (board coordy maxqueens)
	(cond 	((> coordy maxqueens) nil)
			((canbeplaced board coordy 1) (cons coordy board))
			(t (placequeen board (+ 1 coordy) maxqueens))))|#