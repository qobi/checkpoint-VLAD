(define-structure linear-regression beta-hat alpha-hat)

(define (read-data)
 (let loop ((lines
	     (remove-if-not
	      (lambda (line)
	       (or (substring? "Tapenade" line)
		   (substring? "VLAD" line)
		   (substring? "User time (seconds):" line)
		   (substring? "Maximum resident set size (kbytes):" line)))
	      (read-file "run-example.text"))))
  (if (null? lines)
      '()
      (cons (list (string->number (last (fields (first lines))))
		  (string->number (last (fields (third lines))))
		  (string->number (last (fields (second lines)))))
	    (loop (rest (rest (rest lines))))))))

(define (partition-data data samples)
 (if (null? samples)
     (if (null? data) '() (panic "mismatch"))
     (cons (sublist data 0 (first samples))
	   (partition-data (sublist data (first samples) (length data))
			   (rest samples)))))

(define (normalize-data data)
 (map (lambda (r)
       (let ((s (second (first r)))
	     (t (third (first r))))
	(map (lambda (l) (list (first l) (/ (second l) s) (/ (third l) t))) r)))
      data))

(define (linear-regression points)
 ;; http://en.wikipedia.org/wiki/Ordinary_least_squares
 (when (null? points) (panic "No points"))
 (let* ((n (length points))
	(x-sum (map-reduce + 0 x points))
	(x-mean (/ x-sum n))
	(y-sum (map-reduce + 0 y points))
	(y-mean (/ y-sum n))
	(beta-hat (/ (- (map-reduce + 0 (lambda (p) (* (x p) (y p))) points)
			(/ (* x-sum y-sum) n))
		     (- (map-reduce + 0 (lambda (p) (* (x p) (x p))) points)
			(/ (* x-sum x-sum) n))))
	(alpha-hat (- y-mean (* beta-hat x-mean))))
  (make-linear-regression beta-hat alpha-hat)))

(define (fit f input output)
 (let* ((data (map (lambda (line) (map string->number (fields line)))
		   (read-file input)))
	(r (linear-regression
	    (map (lambda (p) (vector (f (first p)) (second p))) data)))
	(beta (linear-regression-beta-hat r))
	(alpha (linear-regression-alpha-hat r)))
  (write-file
   (map (lambda (p)
	 (string-append (number->string (first p))
			" "
			(number->string (+ (* beta (f (first p))) alpha))))
	data)
   output)))

(define (generate-files)
 (for-each
  (lambda (r f)
   (write-file
    (map (lambda (l)
	  (string-append
	   (number->string (first l)) " " (number->string (second l))))
	 r)
    (string-append f "-space.data"))
   (write-file
    (map (lambda (l)
	  (string-append
	   (number->string (first l)) " " (number->string (third l))))
	 r)
    (string-append f "-time.data")))
  (partition-data (read-data) '(7 7 7 7))
  '("a" "b" "c" "d"))
 (fit (lambda (l) l) "a-space.data" "e-space.data")
 (fit (lambda (l) l) "b-space.data" "f-space.data")
 (fit (lambda (l) l) "c-space.data" "g-space.data")
 (fit (lambda (l) (* l (log l))) "d-space.data" "h-space.data")
 (fit (lambda (l) l) "a-time.data" "e-time.data")
 (fit (lambda (l) (expt l (+ 1 (/ 40)))) "b-time.data" "f-time.data")
 (fit (lambda (l) l) "c-time.data" "g-time.data")
 (fit (lambda (l) (* l (log l))) "d-time.data" "h-time.data"))

(generate-files)
