;Assignment 2

;David Lane
;004638553
;Korf/Chen

;3 types of brute-force searches
;NOTE: These functions assume that nil cannot be a leaf node, which seemed clear from the instructions.
;nil can be considered either an atom or an empty list type, which means that these programs do not
;handle cases where nil is a leaf of the tree, it assumes all lists indicate a branch and not an atom
;and that all branches have leaves (as per the definition of a tree)

(defun DFS (lst)					;Depth First Search
	(cond	((null (car lst)) nil)	;Base case, when reaching the end of a tree or branch, return nil		
			((atom (car lst)) (cons (car lst) (DFS (cdr lst)) ))	;if the first item in the list is an atom
																	;append it and look for branches
			(t (append (DFS (car lst)) (DFS (cdr lst))))))	;else the first item is a list, search it and 
															;append its contents to the front of the
															;output list, depth first

;(write (DFS '((A  F (B) ) C (D))))



(defun helper (lst int)				;Auxiliary function for DFID (does constrained DFS)
	(cond	((= int 0) nil)			;Two base cases, when depth to be searched is 0 return nil or
			((null (car lst)) nil)	;when reaching the end of a tree or branch, return nil
			((atom (car lst)) (cons (car lst) (helper (cdr lst) int) ))	;if the first item in the list is an atom
																		;append it and look for branches
			(t (append (helper (car lst) (- int 1)) (helper (cdr lst) int) ))))	;do depth first search,
																				;unless youre at the max depth

(defun DFID (lst int)			;Depth First Iterative Deepening
	(cond	((<= int 0) nil)	;base case, when depth to be searched is 0 return nil
			(t (append (DFID lst (- int 1)) (helper lst int) )))) 	;shell function to append different depths
																	;shallower calls first
;(write (DFID '((A  F (B)) C (D)) 3))



(defun BFS (lst)					;Breadth First Search
	(cond	((null lst) nil)	;Base case, when reaching the end of a tree or branch, return nil
			((atom (car lst)) (cons (car lst) (BFS (cdr lst)) ))	;if the first item in the list is an atom
																	;append it and look for branches
			(t (BFS (append (cdr lst) (car lst))))))	;any time you reach a new branch, you have reached
														;a depth to be searched later, 
														;move it to the end of the lst and search again

(write (BFS '((A  F () ((X)) (B) ) C (D))))

;((A  F () (B) ) C (D))
;(C (D) A  F () (B))
;(C A  F () (B) D)







