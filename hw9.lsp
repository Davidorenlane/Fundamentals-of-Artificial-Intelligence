;Assignment 9

;David Lane
;004638553
;Korf/Chen

;Description: 
;INTERPRET applies a list of production rules to a given expression

(defun INTERPRET (EXP PRODS)	;iterates through production rules until there is no change after one full iteration
	(exhaustive nil (proditerator EXP PRODS) PRODS))

(defun proditerator (EXP PRODS) ;applies each production rule deeply to working memory
	(cond 	((null PRODS) EXP)
			(t (proditerator (applyer EXP (car PRODS)) (cdr PRODS)))))

(defun exhaustive (old new PRODS)	;applies rules until there is no change from applying them
	(cond 	((equal old new) new)
			(t (exhaustive new (proditerator new PRODS) PRODS))))

(defun applyer (EXP rule)	;deeply applies a production rule to working memory
	(cond 	((atom EXP) EXP)
			;if the pattern of working memory matches the rule, we assign to variables and match the right side pattern
			((patternmatch exp (car rule) '1) (assign (car (cdr rule)) (makeassignments (applyall exp rule) (car rule) nil) nil))
			(t (applyall EXP rule))))

(defun patternmatch (exp rule truth)	;matches a pattern in a list in working memory to the left hand side in the rule 
	(cond 	((null truth) truth)
			((null rule) truth)
			((atom (car rule)) (cond 	((isvariable (car rule) VARIABLES) (patternmatch (cdr exp) (cdr rule) truth))
										((atom exp) nil)
										((not (equal (car exp) (car rule))) nil)
										((= (length exp) (length rule)) (patternmatch (cdr exp) (cdr rule) truth))
										(t nil)))
			((atom exp) nil)
			(t (patternmatch (cdr exp) (cdr rule) (patternmatch (car exp) (car rule) '1)))))

(defun isvariable (var lst)	;checks if an atom is a variable defined by VARIABLES
	(cond 	((null lst) nil)
			((eq var (car lst)) var)
			(t (isvariable var (cdr lst)))))

(defun applyall (exp rule)	;helper function for deeply applying production rules
	(cond 	((null exp) nil)
			(t (cons (applyer (car exp) rule) (applyall (cdr exp) rule)))))

(defun makeassignments (exp left assignments)	;given a patterm match, assign variables in left hand side to literals from working memory
	(cond 	((null left) assignments)
			((isvariable (car left) VARIABLES) (makeassignments (cdr exp) (cdr left) (cons (list (car left) (car exp)) assignments)))
			((atom (car left)) (makeassignments (cdr exp) (cdr left) assignments))
			(t (makeassignments (cdr exp) (cdr left) (append (makeassignments (car exp) (car left) nil) assignments)))))

(defun assign (right assignments result)	;takes a list of variable assignments and the right side of a production rule and inserts the corresponding literals
	(cond 	((null right) result)
			((atom (car right)) (assign (cdr right) assignments (append result (list (isin (car right) assignments)))))
			(t (assign (cdr right) assignments (append result (list (assign (car right) assignments nil)))))))

(defun isin (var assignments)	;checks if a variable is in the list of assigned variables made by make assignments
	(cond 	((null assignments) var)
			((eq var (car (car assignments))) (car (cdr (car assignments))))
			(t (isin var (cdr assignments)))))


(defconstant VARIABLES '(X Y Z))
;Production rules for converting to clausal form
(defconstant CLAUSEPRODS '( ((E X Y Z) (A (E X Y) (E X Z) (E Y Z)))
							((E X Y) (A (I X Y) (I Y X))) 
							((I X Y) (O (N X) Y))
							((A X Y Z) (A (A X Y) Z))
							((O X Y Z) (O (O X Y) Z))
							((N (A X Y)) (O (N X) (N Y)))
							((N (O X Y)) (A (N X) (N Y)))
							((O (A X Y) Z) (A (O X Z) (O Y Z)))
							((O X (A Y Z)) (A (O X Y) (O X Z)))
							((N (N X)) (X))
							))

;TESTS

;(write (INTERPRET '(I (N (O p q)) t ) CLAUSEPRODS))
;(write (INTERPRET '(O (A (A X Y) Z) W) CLAUSEPRODS))
;(write (INTERPRET '(d (a (b (b c)))) '(((a x) (b x)) ((b (b (b x))) (X)))))
;(A (I (O s t) q) (I q (O s t)))
;(A (O (N (O s t)) q) (O (N q) (O s t)))
;(A (O (A (N s) (N t)) q) (O (N q) (O s t)))
;(A (A (O (N s) q) (O (N t) q)) (O (N q) (O s t))) 

;(write (atom nil))