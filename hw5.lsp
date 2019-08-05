;Assignment 4

;David Lane
;004638553
;Korf/Chen

;Description:


(defun MAXIMIN (tree alpha beta)
	(cond 	((null tree) (list alpha 0))
			((atom tree) (list tree 1))
			((>= alpha beta) (list alpha 0))
			(t (let ((x (MINIMAX (car tree) alpha beta)))
					 (cond 	((> (car x) alpha)
					 			(list (car (MAXIMIN (cdr tree) (car x) beta)) (+ (car (cdr x)) (car (cdr (MAXIMIN (cdr tree) (car x) beta))))))
							(t 	
								(list (car (MAXIMIN (cdr tree) alpha beta)) (+ (car (cdr x)) (car (cdr (MAXIMIN (cdr tree) alpha beta)))))))))))

(defun MINIMAX (tree alpha beta)
	(cond 	((null tree) (list beta 0))
			((atom tree) (list tree 1))
			((>= alpha beta) (list beta 0))
			(t (let ((x (MAXIMIN (car tree) alpha beta)))
					 (cond 	((< (car x) beta)
					 			(list (car (MINIMAX (cdr tree) alpha (car x))) (+ (car (cdr x)) (car (cdr (MINIMAX (cdr tree) alpha (car x)))))))
							(t 	
								(list (car (MINIMAX (cdr tree) alpha beta)) (+ (car (cdr x)) (car (cdr (MINIMAX (cdr tree) alpha beta)))))))))))


;----------------------------------------------------
#|
(defconstant tree22 '((1 2) (3 4)))

(defconstant tree23 '(((1 2) (3 4)) ((5 6) (7 8))))

(defconstant tree24
'((((1 2) (3 4)) ((5 6) (7 8))) (((9 10) (11 12)) ((13 14) (15 16)))))

(defconstant tree25
'(((((1 2) (3 4)) ((5 6) (7 8))) (((9 10) (11 12)) ((13 14) (15 16))))
  ((((17 18) (19 20)) ((21 22) (23 24)))(((25 26) (27 28)) ((29 30) (31 32))))))

(defconstant tree26 
'((((((1 2)   (3 4))   ((5 6)   (7 8)))  (((9 10)  (11 12)) ((13 14) (15 16))))
   ((((17 18) (19 20)) ((21 22) (23 24)))(((25 26) (27 28)) ((29 30) (31 32)))))
  (((((33 34) (35 36)) ((37 38) (39 40)))(((41 42) (43 44)) ((45 46) (47 48))))
   ((((49 50) (51 52))((53 54) (55 56)))(((57 58) (59 60))((61 62) (63 64)))))))

(defconstant tree32 '((1 2 3) (4 5 6) (7 8 9)))

(defconstant tree33 '(((1 2 3)    (4 5 6)    (7 8 9))
	              ((10 11 12) (13 14 15) (16 17 18))
		      ((19 20 21) (22 23 24) (25 26 27))))

(defconstant tree34 '((((1 2 3)    (4 5 6)    (7 8 9))
                       ((10 11 12) (13 14 15) (16 17 18))
                       ((19 20 21) (22 23 24) (25 26 27)))
                      (((28 29 30) (31 32 33) (34 35 36))
                       ((37 38 39) (40 41 42) (43 44 45))
                       ((46 47 48) (49 50 51) (52 53 54)))
                      (((55 56 57) (58 59 60) (61 62 63))
                       ((64 65 66) (67 68 69) (70 71 72))
                       ((73 74 75) (76 77 78) (79 80 81)))))

(defconstant tree42 '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)))

(defconstant tree43 
'(((1 2 3 4)     (5 6 7 8)     (9 10 11 12) (13 14 15 16))
  ((17 18 19 20) (21 22 23 24) (25 26 27 28) (29 30 31 32))	
  ((33 34 35 36) (37 38 39 40) (41 42 43 44) (45 46 47 48))	
  ((49 50 51 52) (53 54 55 56) (57 58 59 60) (61 62 63 64))))

(defconstant tree52
'((1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20) (21 22 23 24 25)))

(defconstant tree53
'(((1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15)
   (16 17 18 19 20) (21 22 23 24 25))
  ((26 27 28 29 30) (31 32 33 34 35) (36 37 38 39 40) 
   (41 42 43 44 45) (46 47 48 49 50))
  ((51 52 53 54 55) (56 57 58 59 60) (61 62 63 64 65)
   (66 67 68 69 70) (71 72 73 74 75))
  ((76 77 78 79 80) (81 82 83 84 85) (86 87 88 89 90)
   (91 92 93 94 95) (96 97 98 99 100))
  ((101 102 103 104 105) (106 107 108 109 110) (111 112 113 114 115)
   (116 117 118 119 120) (121 122 123 124 125))))

(defconstant tree62 
'((1 2 3 4 5 6)       (7 8 9 10 11 12)    (13 14 15 16 17 18)
  (19 20 21 22 23 24) (25 26 27 28 29 30) (31 32 33 34 35 36))) 
|#

;---------------------------------------------------------------------------

;(write (MAXIMIN '((1 2 3 4 5 6)       (7 8 9 10 11 12)    (13 14 15 16 17 18)
 ; (19 20 21 22 23 24) (25 26 27 28 29 30) (31 32 33 34 35 36)) -1000 1000))
;(write (MAXIMIN '() -1000 1000))
;(write (MAXIMIN '(((4 6) (7 9)) ((1 2) (0 1)) ((8 1) (9 2))) -1000 1000))



;(write (MINIMAX '((1 2) (0 1)) 6 1000))
;(write (MINIMAX '(1 2) -6 1000))
;(write (MAXIMIN '(1 2) -6 1000))

;(write (MAXIMIN '((1 2) (1 1 1)) -6 1000))
;(write (MINIMAX '((1 2) (1 1 1)) -6 1000))
;(write (MINIMAX tree53 -6 1000))
;(write (MAXIMIN tree53 -6 1000))
;
;(write (MINIMAX tree52 -6 1000))
;(write (MAXIMIN tree52 -6 1000))
;
;(write (MINIMAX tree43 -6 1000))
;(write (MAXIMIN tree43 -6 1000))
;
;(write (MINIMAX tree42 -6 1000))
;(write (MAXIMIN tree42 -6 1000))
;
;(write (MINIMAX tree34 -6 1000))
;(write (MAXIMIN tree34 -6 1000))
;
;(write (MINIMAX tree33 -6 1000))
;(write (MAXIMIN tree33 -6 1000))



;SCRAPS



#|(defun MAXIMIN (tree alpha beta)
	(cond 	((null tree) (list 0 alpha))
			((atom tree) (list 1 tree))

			((>= alpha beta)
			 (list 0 alpha ))


			((> (car (cdr (MINIMAX (car tree) alpha beta))) alpha) 
			 	(list (+ (car (MINIMAX (car tree) alpha beta)) (car (MAXIMIN (cdr tree) (car (cdr (MINIMAX (car tree) alpha beta))) beta))) (car (cdr (MAXIMIN (cdr tree) (car (cdr (MINIMAX (car tree) alpha beta))) beta))) ))
			(t (list (+ (car (MINIMAX (car tree) alpha beta)) (car (MAXIMIN (cdr tree) alpha beta))) (car (cdr (MAXIMIN (cdr tree) alpha beta)))))))

(defun MINIMAX (tree alpha beta)
	(cond 	((null tree) (list 0 beta))
			((atom tree) (list 1 tree))

			((>= alpha beta)
			 (list 0 beta ))

			((< (car (cdr (MAXIMIN (car tree) alpha beta))) beta) 
				(list (+ (car (MAXIMIN (car tree) alpha beta)) (car (MINIMAX (cdr tree) alpha (car (cdr (MAXIMIN (car tree) alpha beta)))))) (car (cdr (MINIMAX (cdr tree) alpha (car (cdr (MAXIMIN (car tree) alpha beta))) )))))
			(t (list (+ (car (MAXIMIN (car tree) alpha beta)) (car (MINIMAX (cdr tree) alpha beta))) (car (cdr (MINIMAX (cdr tree) alpha beta)))))))|#


#|(defun MAXIMIN (tree alpha beta)
	(cond 	((null tree) (list nil alpha))
			((atom tree) (list (list tree) tree))

			((>= alpha beta)
			 (list nil alpha ))


			((> (car (cdr (MINIMAX (car tree) alpha beta))) alpha) 
			 	(list 	(append (car (MINIMAX (car tree) alpha beta)) 
			 					(car (MAXIMIN (cdr tree) (car (cdr (MINIMAX (car tree) alpha beta))) beta)))
			 			(car (cdr (MAXIMIN (cdr tree) (car (cdr (MINIMAX (car tree) alpha beta))) beta))) ))
			(t (list (append (car (MINIMAX (car tree) alpha beta)) (car (MAXIMIN (cdr tree) alpha beta))) (car (cdr (MAXIMIN (cdr tree) alpha beta)))))))

(defun MINIMAX (tree alpha beta)
	(cond 	((null tree) (list nil beta))
			((atom tree) (list (list tree) tree))

			((>= alpha beta)
			 (list nil beta ))

			((< (car (cdr (MAXIMIN (car tree) alpha beta))) beta) 
				(list 	(append (car (MAXIMIN (car tree) alpha beta))
				 				(car (MINIMAX (cdr tree) alpha (car (cdr (MAXIMIN (car tree) alpha beta)))))) 
						(car (cdr (MINIMAX (cdr tree) alpha (car (cdr (MAXIMIN (car tree) alpha beta))) )))))
			(t (list (append (car (MAXIMIN (car tree) alpha beta)) (car (MINIMAX (cdr tree) alpha beta))) (car (cdr (MINIMAX (cdr tree) alpha beta)))))))|#




#|(defun MAXIMIN (tree alpha beta)
	(cond 	((null tree) (list 0 alpha))
			((atom tree) (list 1 tree))
			((> (car (cdr (MINIMAX (car tree) alpha beta))) (car (cdr (MAXIMIN (cdr tree) alpha beta)))) 
				(list (+ (car (MINIMAX (car tree) alpha beta)) (car (MAXIMIN (cdr tree) alpha beta))) (car (cdr (MINIMAX (car tree) alpha beta))))) 
			(t (list (+ (car (MINIMAX (car tree) alpha beta)) (car (MAXIMIN (cdr tree) alpha beta))) (car (cdr (MAXIMIN (cdr tree) alpha beta)))))))

;tree :: ((tree) nodes searched)
(defun MINIMAX (tree alpha beta)
	(cond 	((null tree) (list 0 beta))
			((atom tree) (list 1 tree))
			((< (car (cdr (MAXIMIN (car tree) alpha beta))) (car (cdr (MINIMAX (cdr tree) alpha beta)))) 
				(list (+ (car (MAXIMIN (car tree) alpha beta)) (car (MINIMAX (cdr tree) alpha beta))) (car (cdr (MAXIMIN (car tree) alpha beta)))))
			(t (list (+ (car (MAXIMIN (car tree) alpha beta)) (car (MINIMAX (cdr tree) alpha beta))) (car (cdr (MINIMAX (cdr tree) alpha beta)))))))|#

