(import (chicken eval)
	(chicken format)
	(chicken process-context)
	(chicken io)
	(prefix libdxtr dxtr:))

;; it's not pretty, but it works
(define roll dxtr:roll)
(define roll-many dxtr:roll-many)
(define roll-one-or-many dxtr:roll-one-or-many)
(define dice-op dxtr:dice-op)
(define oracle dxtr:oracle)
(define portent dxtr:portent)

(define (help)
  (print "dxtr - the commandline dice roller")
  (print "usage: dxtr [-i|-h] expression")
  (newline)
  (print "commandline args")
  (print "-i\tenter an interactive repl")
  (print "-x\texpand expressions to their individual rolls")
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

(define (dxtr-eval str)
  (let ((parse-result (dxtr:dxtr-parse str)))
    (print (if parse-result
	       (let ((res (eval parse-result)))
		 (if (list? res)
		     (display-result res)
		     ""))
	       "Parser error"))))

(if interactive
    (dxtr-repl)
    (dxtr-eval (car (reverse (command-line-arguments)))))

