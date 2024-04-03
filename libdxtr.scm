(module libdxtr (roll roll-many roll-one-or-many dice-op oracle portent dxtr-parse)
  (import scheme
	  (chicken base)
	  (chicken eval)
	  (chicken io)
	  (chicken platform)
	  (chicken random)
	  comparse
	  srfi-1
	  srfi-14)

  (define (roll sides)
    (+ 1 (pseudo-random-integer sides)))

  ;; roll-one and roll-many roll one or many dice
  ;; and return a list of form:
  ;; (roll-result (sides) ((individual-results ...)))
  (define (roll-one sides)
    (let ((roll-result (roll sides)))
      (list roll-result (list sides) (list (list roll-result)))))

  (define (roll-many sides num)
    (define (r i acc rolls)
      (if (> i 0)
	  (let ((roll-result (roll sides)))
	    (r (- i 1)
	       (+ acc roll-result)
	       (append rolls (list roll-result))))
	  (list acc (list sides) (list rolls))))
    (r num 0 '()))

  (define (roll-one-or-many sides num)
    (if (and num (> num 1))
	(roll-many (or sides 6) num)
	(roll-one (or sides 6))))

  (define (oracle)
    (let ((r (roll 6)))
      (list (cond
	((= r 1) "No, and...")
	((= r 2) "No")
	((= r 3) "No, but...")
	((= r 4) "Yes, but...")
	((= r 5) "Yes")
	((= r 6) "Yes, and...")))))

  (define words-location
    "/usr/share/dict/words")

  (define words #f)

  (define (filter-word x)
    (and (> (string-length x) 1)
	 (not (member #\' (string->list x)))))

  (define (set-words)
    (set! words (filter filter-word
			(call-with-input-file words-location
			  (lambda (port) (read-lines port)))))
    words)
  
  (define (get-words)
    (or words
	(set-words)))

  (define (get-word-linux)
   (list-ref (get-words)
	     (pseudo-random-integer (length words))))
  
  (define (portent)
    (list (cond-expand
	    (linux (get-word-linux))
	    (else "Not supported by this platform"))))

  (define (normalize-literal x)
    (if (list? x)
	x
	(list x `() `())))

  ;; Operations on roll results
  (define (dice-op op r1 r2)
    (let ((res1 (normalize-literal r1))
	  (res2 (normalize-literal r2)))
      (list (eval (list op (car res1) (car res2))
		  (module-environment 'libdxtr))
	    (append (cadr res1) (cadr res2))
	    (append (caddr res1) (caddr res2)))))

;;; Parser
  ;; this is shit, write a proper parser here
  ;; Not working:
  ;; - operator precedence
  ;; - brackets
  (define digit
    (in char-set:digit))

  (define digits
    (as-string (one-or-more digit)))

  (define opt-digits
    (as-string (zero-or-more digit)))

  (define dice-group
    (sequence* ((num opt-digits)
		(_ (is #\d))
		(sides opt-digits))
	       (result (list 'roll-one-or-many
			     (string->number sides)
			     (string->number num)))))

  (define num-literal
    (sequence* ((num digits))
	       (result (string->number num))))

  (define literal
    (any-of dice-group num-literal))

  (define expr
    (any-of literal))

  (define binop
    (sequence* ((e1 expr)
		(op (in (string->char-set "+-*/")))
		(e2 expr))
	       (result (list 'dice-op
			     (string->symbol
			      (list->string (list op)))
			     e1
			     e2))))

  (set! expr
    (any-of
     binop
     literal))

  (define keyword
    (sequence* ((x (any-of
		    (char-seq "oracle")
		    (char-seq "portent")
		    (char-seq "help")
		    (char-seq "exit"))))
	       (result (list (string->symbol x)))))

  (define top-level
    (any-of
     keyword
     expr))

  (define (dxtr-parse x)
    (parse top-level x))
  )
