;Assignment 7

;David Lane
;004638553
;Korf/Chen

;Description: This code will solve the towers of hanoi puzzle for any start and goal representation and any number n rings. The logic of the recursion
;is that in order to move the biggest ring from peg X to peg Y, all the other rings must be on the third peg, Z. To do this, for each move, we write the 
;moves required to move all but the largest ring to a stack on peg Z ++ the move representation for moving the largest ring from X to Y ++ the moves
;required to move the other rings from their position in a stack on peg Z to the remaining goal. 

(defun hanoi (lst1 lst2)    ;takes two lists of n Symbols from the set {A,B,C}, the first is the start state, the second is the goal state
							   ;the first symbol in each of these lists is the location of the largest ring, the second is the second largest ring
							   ;and so on with the last being the smallest ring
	(hanoier lst1 lst2 (listlength lst1 0)))

(defun listlength (lst l) ;does exactly what the predefined lisp function "length" does
	(cond  	((null lst) l)
			(t (listlength (cdr lst) (+ l 1)))))

(defun otherofABC (char1 char2) ;given two of A, B, or C, returns the other
	(cond 	((eq char1 'A)  (cond 	((eq char2 'B) 'C)
									(t 'B)))
			((eq char1 'B)  (cond 	((eq char2 'A) 'C)
									(t 'A)))
			(t 			 	(cond 	((eq char2 'A) 'B)
									(t 'A)))))

(defun listofnXs (n x lst) ;creates a list of n items, all equal to x
	(cond 	((= n 0) lst)
			(t (listofnXs (- n 1) x (cons x lst)))))

(defun hanoier (lst1 lst2 n) ;Core Function, takes two state representations and their size.
	(cond 	((null lst1) nil) ;base case, if we have hanoi subproblem of size 0, no moves are necessary to solve
			((eq (car lst1) (car lst2)) (hanoier (cdr lst1) (cdr lst2) (- n 1))) ;If the largest disc is in place, we can move to a smaller subproblem
			(t  (let 	((stack (listofnXs (- n 1) (otherofABC (car lst1) (car lst2)) nil))) ;create a state representation for a stack of n-1 rings on one peg
						(append (hanoier (cdr lst1) stack (- n 1))   ;generate the moves necessary to move all smaller rings to one peg and append those moves to
								(list (list n (car lst1) (car lst2))) ;to the representation of the move of the largest disc
								(hanoier stack (cdr lst2) (- n 1))))))) ;move rings from their position on one peg to the remaining goal state


;TESTS

(defun test1 ()
	(hanoi '(A B C A) '(C B A C)))
(defun test2 ()
	(hanoi '(C B A) '(A B C)))
(defun test3 ()
	(hanoi '(A A A) '(C C C)))
(defun test4 ()
	(hanoi '(A B C C B) '(C B A A B)))
(defun test5 ()
	(hanoi '(A B C A B C) '(A C B C B A)))
(defun test6 ()
	(hanoi '(B A B A B A) '(A B A B A B)))
(defun test7 ()
	(hanoi '(A B B C C C) '(A C C A B A)))
(defun test8 ()
	(hanoi '(A A A A A A A) '(C C C C C C C)))
(defun test9 ()
	(hanoi '(C C B B B A A A A) '(C C B B B A A A A)))
(defun test10 ()
	(hanoi '() '()))
(defun test11 ()
	(hanoi '(B B B A A A A) '(A C A C A C A)))
(defun test12 ()
	(hanoi '(B C C B B A A A) '(C A B C B A B C)))

;(write (listlength (test6) 0))
;(write (test12))




;Scraps

#|(defun samelists (lst1 lst2 truth) ;takes two lists of same length, returns true if they are the same
	(cond 	((null lst1) truth)
		 	((eq (car lst1) (car lst2)) (samelists (cdr lst1) (cdr lst2) truth))
		 	(t nil)))|#