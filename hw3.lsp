;Assignment 3

;David Lane
;004638553
;Korf/Chen

;NOTE: This solution does not work on lists containing negative integers. The problem spec implies that we will 
;use (random n) to generate integers meaning all integers will be zero or greater. We also have generally not considered 
;negative edge costs etc. in class.

;Explanation

;This function works by first converting the input list to a list in solution form specified by the problem,
;then generates a new layer of nodes by shifting all possible atoms from the first list in the partition.
;it then adds those nodes to the list of nodes to be searched, and continues with depth first search. It searches all
;nodes and creates a leaf when it finds an optimal solution or the second list is larger than
;the first in the created node

;Auxiliary Functions

(defun absvaldif (x y) ;returns absolute value of the dif of two ints
	(cond 	((= x y) 0)
			((> x y) (- x y)) 
			((< x y) (- y x))))
		 									
(defun summer (lst) ;returns the sum of the members of a list
	(cond 	((null lst) 0)
			(t (+ (car lst) (summer (cdr lst)) ))))

(defun largest (int lst) ;returns the largest member of a list
	(cond 	((null lst) int)
			((> (car lst) int) (largest (car lst) (cdr lst)))
			(t (largest int (cdr lst)))))

(defun remover (int lst) ;removes first instance of int from lst
	(cond 	((= int (car lst)) (cdr lst) )
			 (t (cons (car lst) (remover int (cdr lst)) ))))


;Juicier Functions more specific to this program

(defun comper (lst1 lst2) ;compares the difference values for two partition lists, returns the better of the two
	(cond 	((null lst1) lst2)
			((null lst2) lst1)
			((> (car lst1) (car lst2) ) lst2)
			((<= (car lst1) (car lst2) ) lst1)))

(defun shifter (int lst) ;shifts int from first sublist in lst to second sub list, updates difference 
	(list (absvaldif (summer (remover int (car (cdr lst)))) (summer (cons int (car (cdr (cdr lst))))) )
		  (remover int (car (cdr lst)))
		  (cons int (car (cdr (cdr lst)))) ))

(defun starter (lst) ;moves lst into starting form for recurser
	(shifter (largest 0 lst) (list (summer lst) lst nil ) ))

(defun allgreater (lst int notonesmall)
	(cond 	((null lst) notonesmall)
			((< (car lst) int) (allgreater (cdr lst) int (not t)))
			(t (allgreater (cdr lst) int notonesmall))))

(defun onemore (int tocheck result)
	(cond 	((null tocheck) result)
			((> (car tocheck) int) (cond 	((> (car tocheck) (* int 4)) (onemore int (cdr tocheck) result))
											((null result) (onemore int (cdr tocheck) (cons (car tocheck) result)))
											((< (car tocheck) (car result)) (onemore int (cdr tocheck) (cons (car tocheck) (cdr result))))
											(t (onemore int (cdr tocheck) result))))
			(t (onemore int (cdr tocheck) result))))

(defun allless (int tocheck result)
	(cond 	((null tocheck) result)
			((= (car tocheck) int) (list int))
			((> (car tocheck) int) (allless int (cdr tocheck) result))
			(t (allless int (cdr tocheck) (cons (car tocheck) result)))))


(defun alllessonemore (int lst) ;returns a list with one of each number in lst less than int as well as the smallest number bigger than int in lst
	(append (onemore int lst nil) (allless int lst nil)))
	;lst)
								;an algorithm like this could be implemented to constrain which new nodes get generated, for now I have 
								;not added this, as I spoke with the professor in office hours, and he said the only constraint was that 
								;the depth of search be less than number of integers which my program satisfies

								;By limiting the numbers to be shifted from the first list to the second to generate new nodes
								;to only the numbers less than half of the difference of parent node
								;and the smallest number greater than half of the difference, we can reduce the branching factor of the
								;algorithm, avoid repeats, and improve performance,

								;To implement this one could eliminate duplicates from the toshift list, sort, and then return the proper numbers
								;easily

#|(defun eliminateDuplicates (L)
  (cond ((null L) L)
        ((member (car L) (cdr L))
         (eliminateDuplicates (cdr L)))
        (t (cons (car L) (eliminateDuplicates (cdr L))))))|#			
	

(defun generator (toshift part result) ;generates new nodes for a new level of depth in the search tree. shifts each possible number from 
										;the first list in partition to the second list in partition
	(cond 	((null toshift) result)
			(t (generator (cdr toshift) part (cons (shifter (car toshift) part) result ) )))) 

(defun genchildren (lst) ;surface function for generating new nodes to be searched
	(generator (alllessonemore (/ (car lst) 2) (car (cdr lst)) ) lst nil) )			 							

(defun recurser (best remaining) ;searches nodes, depth first, updates best when a better node is found
	(cond 	((null remaining) best)
			((<= (car best) 1) best) ;given two way partition and integers, if we find diff 1 or less we know we have optimal partition
			;((allgreater (car (cdr (car remaining))) (* (car (car remaining)) 2) t) (recurser best (cdr remaining)))
			((> (summer (car (cdr (cdr (car remaining))))) (summer (car (cdr (car remaining)))) ) (recurser (comper (car remaining) best ) (cdr remaining) ) )
			(t 	(recurser (comper (car remaining) best ) (append (genchildren (car remaining)) (cdr remaining)) ))))

					#|(cond 	((= 1 (car x)) (recurser (shifter (car (car (cdr x))))))
							((= 2 (car x)) (comper (recurser (shifter (car (car (cdr x))))) (recurser (shifter (car (cdr (car (cdr x)))))))))|#

(defun PARTITION (lst) ;main function
	(cond 	((null lst) '(0 nil nil))
			(t (recurser (starter lst) (genchildren (starter lst)) ))))
			

;For generating test cases

(defun testcase (q val lst) ;constructs a list of q numbers from 0 to val
	(cond 	((= q 0) lst)
			(t (testcase (- q 1) val (cons (random val) lst) ))))
			
;Test Cases

;(write (remover '1 '(1 2 3 1)) )
;(write (summer '(1 2 3)) )
;(write (absvaldif '1 '3) )
;(write (absvaldif '3 '1) )
;(write (shifter 1 '((1 2 3) () 4) ))
;(write '(1 2 3))
;(write (> '() 1))
;(write (largest 0 '(1 2 3 1)))
;(write (comper (starter '(1 2 3)) (starter '(1 2 9))))
;(write (car (car (cdr '(2 (1)) ))))
;(write (PARTITION '(1 2 3 8)))
;(write (starter '(1 2 3)))
;(write (PARTITION '(1 2 3 4 5 10 19 32)))
;(write (bubsortdestroy '(1 3 7 3 2 4 5)))
(write (PARTITION (testcase 10 10000 nil)))

;(write (PARTITION '(7727 7741 7753 7757 7759 7789 8893 8817 7823 7829 7841 7909 7441 6661 6131 7351 6121 6113 6737 6571)))