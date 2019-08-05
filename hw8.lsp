;Assignment 8

;David Lane
;004638553
;Korf/Chen

;Description:
;Resolution theorem prover for propositional logic. 
;Takes a knowledge base and returns a proof tree if a contradiction exists, or FAIL if not.

(defun refute (KB)  ;Top-Level function, calls recursive fuction on prepared data
	(refuter (prepper KB)))

(defun prepper (KB)	;Removes clauses from KB that are (always) true and makes remaining clauses into lists so they can be compared
	(cond 	((null KB) nil)				;more easily with clauses in later iterations that are the roots of subtrees of the proof
			((removeiftrue (car KB)) (prepper (cdr KB)))
			(t (cons (list (car KB)) (prepper (cdr KB)))))) 

(defun removeiftrue (clause) ;removes a clause if it evaluates to true (has two contradicting literals)
	(cond 	((null clause) nil)
			((contradictsany (car clause) (cdr clause)) t)
			(t (removeiftrue (cdr clause)))))




(defun refuter (KB) 		;Recursive function
	(cond 	((contradictionin KB) (contradictionin KB)) ;if we have a contradiction in the knowledge base, return the contradiction
			((< (length KB) 2) 'Fail)					;in this case there is no possibility of a contradiction existing
		 	(t (refuter (append (cdr KB) (resolvehelper (car KB) (cdr KB))))))) ;compares first item to all other items, in KB
																				;appends the results of these comparisons to the end of the KB and
																				;calls refuter again recursively on the resulting knowledge base


(defun contradictionin (KB) ;checks if any item in a KB evaluates to false, which would indicate a contradiction
	(cond 	((null KB) nil)											;and returns that item
			((null (car (car (car KB)))) (car KB))
			(t (contradictionin (cdr KB)))))

(defun resolvehelper (clause remaining)	;checks for a given knowledge base, what contradictions can be made comparing the first clause to all remaining clauses
	(cond 	((null remaining) nil)
			(t (append (logic clause (car remaining)) (resolvehelper clause (cdr remaining))))))

(defun logic (clause1 clause2)
	(let 	((x (removecontras (append (car clause1) (car clause2))))) ;apply resolution on two clauses
	(cond 	((= (length x) (- (+ (length (car clause1)) (length (car clause2))) 2)) ;if only one character was removed from each clause
				(list (list (removedupliterals x) clause1 clause2)) ) ;we have a valid example of the resolution rule, and we add this to our knowledge base for later use 
			(t nil)))) 													;we put this new clause in the form (newclause (oldclause1) (oldclause2))

(defun removecontras (clauses) 	;removes all contradicting items from two appended clauses
	(cond 	((null clauses) nil)
			((contradictsany (car clauses) (cdr clauses)) (removecontras (removeliteral (contradictsany (car clauses) (cdr clauses)) (cdr clauses))))
			(t (cons (car clauses) (removecontras (cdr clauses))))))

(defun removedupliterals (clause) 	;removes redundant duplicate literals from a clause
	(cond 	((null clause) nil)
			(t (cons (car clause) (removedupliterals (removeliteral (car clause) (cdr clause)))))))


(defun removeliteral (literal clause) ;removes a given literal from a clause
	(cond 	((null clause) nil)
			((equal literal (car clause)) (removeliteral literal (cdr clause)))
			(t (cons (car clause) (removeliteral literal (cdr clause))))))

(defun contradictsany (literal clause) ;checks if given literal contradicts any literal in a clause
	(cond 	((null clause) nil)
			((contradicts literal (car clause)) (car clause))
			(t (contradictsany literal (cdr clause)))))

(defun contradicts (literal1 literal2)	;checks if two literals contradict eachother, if so, returns the second literal, else nil
	(cond 	((atom literal1) (cond 	((atom literal2) nil)
									((eq literal1 (car (cdr literal2))) literal2)
									(t nil)))
			(t 				 (cond 	((atom literal2) (cond 	((equal literal2 (car (cdr literal1))) literal2)
															(t nil)))
									(t nil)))))






;TESTS

;(write (equal '(y (x)) '(y (j))))
;(write (refute '(((not a) b) ((not b)) ((not c) d) ((not d)) (a c))))
;(write (refute '(((not a) (not a) b) ((not b) b) ((not c) d d) ((not d)) ((not b)) (a c))))
#|(write (refute
   '(((not mon) (not tue))
     ((not tue) (not wed))
     ((not tue) (not fri))
     (mon wed (not lecture))
     (fri (not recitation))
     (lecture recitation (not class)) 
     (tue)
     (class)))
)|#

;(write (refute '((a) ((not a)))))

;(write (refute '(((not rainy) (not sunny)) (sunny) (rainy))))

;(write (refute '((a) (b) (a (not b)))))
; FAIL

;(write (refute '(((not study)) (study (not pass)) (pass (not graduate)) (graduate))))

;(write (refute '((study) (study (not pass)) (pass (not graduate)) ((not graduate)))))
 ;FAIL

#|(write (refute
   '((p)
     ((not p) (not q) r)
     ((not s) q)
     ((not t) q)
     (t)
     ((not r)))))
|#

;(write (refute '((a) (b) (a (not b)))))

;(write (lister '((a) ((not a)))))



;Scraps

;(defun resolve (KB)
;	(cond 	((null KB) nil)
;			((resolvehelper (car KB) (cdr KB)) (append (resolvehelper (car KB) (cdr KB)) (resolve (cdr KB))))
;			(t (cons (car KB) (resolve (cdr KB))))))