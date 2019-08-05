;Assignment 1

;David Lane
;004638553
;Korf/Chen

;Overall Explanation
#| The first function is a near brute force way of generating fibonacci numbers. It achieves this in a style quite analogous to 
the definition of fibonacci numbers, by simply recursively adding the previous two, which it in turn also generates recursively.

The second function returns the number of additions needed by the first function, by tracing back in the same way that the FIB
function does and adding 1 every time until it reaches the base cases

The third function is a helper function which returns a list of fibonacci numbers very efficiently, in linear time. It does so by memoizing 
the previous list and then using car and cdr to look at and add the first two elements, appending this as a new element

The FASTFIB function then only needs to take the first item of the output from a call to helper |#

(defun FIB (x)				; ATOM (INT) -> ATOM (INT)
	(cond 	((<= x 0) 0)	; base cases, these are the first numbers of the fibonacci series which are to be summed
			((<= x 2) 1)	
			(t(+ (FIB (- x 1)) (FIB (- x 2))))))	; analagous to the definition of the fibonacci, this method
													; sums the previous and pre-previous Fibonacci number, which it
													; generates by calling the function recursively

(defun SUMS (x)				; ATOM (INT) -> ATOM (INT)
	(cond 	((<= x 2) 0)	; if FIB hits only the base cases, it requires no addition
			(t(+ 1 (SUMS (- x 1)) (SUMS (- x 2))))))	; FIB requires 1 addition each time it is called, hence +1
														; adding the results of the recursive calls mimics the 
														; recursive structure of the FIB function 

(defun HELPER (x)			; ATOM (INT) -> LIST  Returns list with x fibonacci numbers, high to low
	(cond 	((= x 0) nil)		; base cases, these are lists of the first numbers of the fibonacci series 
			((= x 1) '(1))		; which are to be summed
			((= x 2) '(1 1))
			(t 	(let((lst (HELPER (- x 1))))		; binds lst to output of last iteration of the function (recursive call)
					(cons (+ (car lst) (car(cdr lst))) lst)))))	; constructs a new list, appending the sum of the first 
																; two items to the list from the previous iteration

(defun FASTFIB (x)			; ATOM (INT) -> ATOM (INT)
	(car (HELPER x)))		; returns the first number in the list (series) returned by helper, the xth Fibonacci number


;TESTING & QUESTIONS
;NOTE: I tested all required cases, and then removed all but one test case for readability

;Question 1

;(write(FIB 20))
#| first 20 all came out correctly |#

;(Write(FIB 1000))
#| The program appears to stop. I ran (load "hw1.lsp") including the (uncommented) line above and it hangs after

;; loading file hw1.lsp ...
_

I try uncommenting (write(FIB 20)) and run again
This time it writes:

;; loading file hw1.lsp ...
6765_ 

and then hangs. I try again this time replacing (FIB 1000) with (FIB 30)
The program takes a second but eventually prints the 30th Fibonacci number. This leads us to believe that the program
is not timing out, but merely taking a while to execute because it is very inefficient, the run time is exponential |#


;Question 2

;(write(SUMS 10))

#| I tested with the first 10 cases, as well as:
(write(FIB 15))
(write(SUMS 15))
(write(FIB 20))
(write(SUMS 20))

Relationship
(SUMS X) = (FIB X) - 1

In my implementation, this is the case because for all calls to FIB, the program will essentially only add ones together.
It recursively works its way down in a tree pattern and adds the nodes together at the bottom before it can do additions higher up
	3
   / \ 
   2  1
  / \	 
  1  1

So to get to the number 3, it must add 1+1 and then 2+1, or effectively (1+1)+1. To get the next number, 5
the program would have to do 1+1 and then 2+1 again to get 3, and then another 1+1 to get 2, and then 2+3, or effectively ((1+1)+1)+(1+1).
If we look at this pattern we can see that the number of sums in between the 1s would therefore be one less than the number of 1s |#


;Question 3

;(write (FASTFIB 100))

;(time (FIB 20))
#| output:

;; loading file hw1.lsp ...
Real Time: 0.015647 sec.
Run Time: 0.015625 sec.
Space: 0 Bytes
;; loaded file hw1.lsp
T

Also tried with: 

(time (FIB 1)) = 0
(time (FIB 5)) = 0 
(time (FIB 10)) = 0
(time (FIB 15)) = 0
(time (FIB 19)) = 0
(time (FIB 21)) = 0.03125 sec.
(time (FIB 22)) = 0.0625 sec.

The first 5 were too quick to register, but the doubling run times from x = 20,21,22 suggest that the run time is exponential, 2^n.
we can estimate that the run time for (FIB 100) would be (FIB 20) * 2^80 or ~ 1.89 * 10^22 seconds, around 6 trillion years. WOW. |#