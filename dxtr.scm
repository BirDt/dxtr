(import (chicken eval)
	(chicken format)
	(chicken process-context)
	(chicken io)
	(chicken sort)
	(prefix libdxtr dxtr:)
	srfi-1)

;; it's not pretty, but it works
(define roll dxtr:roll)
(define roll-many dxtr:roll-many)
(define roll-one-or-many dxtr:roll-one-or-many)
(define dice-op dxtr:dice-op)
(define oracle dxtr:oracle)
(define portent dxtr:portent)
(define ip dxtr:ip)
(define ip+ dxtr:ip+)
(define ip- dxtr:ip-)
(define intervene dxtr:intervene)
(define npc dxtr:npc)
(define twene dxtr:twene)

(define (help)
  (print "dxtr - the commandline dice roller")
  (print "usage: dxtr [-i|-h] expression")
  (newline)
  (print "commandline args")
  (print "-i\tenter an interactive repl")
  (print "-x\texpand expressions to their individual rolls")
  (print "-s\tsimulate the same expression a few million times and return how many")
  (print "-h\tshow this text")
  (newline)
  (print "expression language")
  (print "example: 2d6+1")
  (print "supported operations: + - * /")
  (print "note: operations have no precedence")
  (newline)
  (print "standalone expression keywords")
  (print "oracle\tMUNE oracle roll")
  (print "portent\tprint a random word")
  (print "ip\tprint current intervention points")
  (print "ip+ | ip-\t increment/decrement intervention points")
  (print "intervene\troll on the interventions table, reset ip")
  (print "npc\troll on the npc interactions table")
  (print "twene\troll on the twene table")
  (print "help\tshow this text")
  (print "exit\tquit the repl, synonym for ctrl-c or ctrl-d"))

(when (or (= (length (command-line-arguments)) 0)
	  (member "-h" (command-line-arguments)))
  (help)
  (exit))

(define interactive
  (member "-i" (command-line-arguments)))

(define expand-rolls
  (member "-x" (command-line-arguments)))

(define simulate
  (member "-s" (command-line-arguments)))

(define (dxtr-repl)
  (display "roll> ")
  (let ((inp (read-line)))
    (when (equal? #!eof inp)
      (newline)
      (exit))
    (dxtr-eval inp)
    (dxtr-repl)))

(define (display-result x)
  (let ((out-str (car x))
	(i 0))
    (when (and expand-rolls (list? x) (= 3 (length x)))
      (set! out-str (number->string out-str))
      (for-each (lambda (y)
		  (set! out-str (string-append out-str
					       (sprintf "\n~Sd~S: ~S"
							(length (list-ref (caddr x) i))
							y
							(list-ref (caddr x) i))))
		  (set! i (+ 1 i)))
		(cadr x)))
    out-str))

(define (dxtr-immediate-eval parse-result)
   (print (if parse-result
	       (let ((res (eval parse-result)))
		 (if (list? res)
		     (display-result res)
		     ""))
	       "Parser error")))

(define simulation-num 10000)

(define (get-frequencies x)
  (let* ((processed x)
	 (counts '())
	 (count-elem (lambda (elem)
		       (length (filter (lambda (y) (= y elem)) processed))))
	(remove-elem! (lambda (elem)
			(set! processed (filter (lambda (y) (not (= y elem))) processed)))))
    (define (iter)
      (if (or (= 0 (length processed)) (null? processed))
	  counts
	  (let ((elem (last processed)))
	    (set! counts (append counts (list (list elem (count-elem elem)))))
	    (remove-elem! elem)
	    (iter))))
    (list (length x) (sort (iter)
			   (lambda (x1 x2) (< (car x1) (car x2)))))))

(define (dxtr-simulate parse-result)
  (let ((freqs (get-frequencies (map (lambda (x) (car (eval parse-result))) (make-list simulation-num)))))
    (for-each (lambda (x)
		(printf "~S: ~S%~N" (car x) (exact->inexact (* 100 (/ (cadr x) (car freqs))))))
	      (cadr freqs))))

(define (dxtr-eval str)
  (let ((parse-result (dxtr:dxtr-parse str)))
    (if simulate
	(dxtr-simulate parse-result)
	(dxtr-immediate-eval parse-result))))

(if interactive
    (dxtr-repl)
    (dxtr-eval (car (reverse (command-line-arguments)))))

