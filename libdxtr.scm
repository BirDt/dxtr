(module libdxtr (roll roll-many roll-one-or-many dice-op ip ip+ ip- intervene oracle npc twene portent dxtr-parse)
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

  ;; MUNE oracle stuff

  (define intervention-points (make-parameter 0))

  (define (ip)
    (list (intervention-points)))

  (define (ip+)
    (intervention-points (+ 1 (intervention-points)))
    (ip))

  (define (ip-)
    (intervention-points (- (intervention-points) 1))
    (ip))

  (define (intervene)
    (intervention-points 0)
    (let ((r (roll 6)))
      (list (cond
	     ((= r 1) "New entity")
	     ((= r 2) "Entity positive")
	     ((= r 3) "Entity negative")
	     ((= r 4) "Advance plot")
	     ((= r 5) "Regress plot")
	     ((= r 6) "Wild")))))
  
  (define (oracle)
    (let ((r (roll 6)))
      (list (string-append
	     (cond
	      ((= r 1) "No, and...")
	      ((= r 2) "No")
	      ((= r 3) "No, but...")
	      ((= r 4) "Yes, but...")
	      ((= r 5) "Yes")
	      ((= r 6) (ip+)
	       "Yes, and..."))
	     (if (> (intervention-points) 2)
		 " - INTERVENTION"
		 "")))))

  (define (npc)
    (let ((r (roll 3)))
      (list (cond
	     ((= r 1) "Hostile")
	     ((= r 2) "Neutral")
	     ((= r 3) "Friendly")))))

  (define (twene)
    (let ((r (roll 10)))
      (list (cond
	     ((= r 1) "Increase simple element")
	     ((= r 2) "Decrease simple element")
	     ((= r 3) "Add simple element")
	     ((= r 4) "Remove simple element")
	     ((= r 5) "Increase major element")
	     ((= r 6) "Decrease major element")
	     ((= r 7) "Add major element")
	     ((= r 8) "Remove major element")
	     ((= r 9) "Wild positive")
	     ((= r 10) "Wild negative")))))

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
      (list (exact->inexact (eval (list op (car res1) (car res2))
				  (module-environment 'libdxtr)))
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
		    (char-seq "ip-")
		    (char-seq "ip+")
		    (char-seq "ip")
		    (char-seq "intervene")
		    (char-seq "npc")
		    (char-seq "twene")
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
