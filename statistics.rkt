(define (sum data-set)
  (cond ((null? data-set) 0)
        (else (+ (car data-set) (sum (cdr data-set))))))

(define (mean data-set)
  (* 1.0 (/ (sum data-set) (length data-set))))

(define (variance data-set)
  (/ (sum (map (lambda (x) (expt (- x (mean data-set)) 2)) data-set)) (length data-set)))

(define (standard-dev data-set)
  (sqrt (variance data-set)))

(define (item index l)
  (cond ((= index 0) (car l))
        (else (item (- index 1) (cdr l)))))


(define (filter pred? l)
  (cond ((null? l) '())
        ((pred? (car l)) (cons (car l) (filter pred? (cdr l))))
        (else (filter pred? (cdr l)))))

(define (sort data-set)
  (define (sorter data-set sorted)
    (cond ((null? data-set) sorted)
          ((equal? data-set (filter (lambda (x) (<= (car data-set) x)) data-set))
           (sorter (cdr data-set) (append sorted (list (car data-set)))))
          (else (sorter (append (cdr data-set) (list (car data-set))) sorted))))
  (sorter data-set '()))

(define (median data-set)
  (let ((n (length data-set))
        (sort-set (sort data-set)))
    (cond ((= (remainder n 2) 0) (mean (list (item (- (/ n 2) 1) sort-set) (item (/ n 2) sort-set))))
          (else (item (floor (/ n 2)) sort-set)))))

(define (minimum data-set)
  (apply min data-set))

(define (maximum data-set)
  (apply max data-set))

(define (q-med sort-set)
  (let ((n (length sort-set)))
    (cond ((= 0 (remainder n 4))
           (mean (list (item (/ n 4) sort-set) (item (- (/ n 4) 1) sort-set))))
          ((= 1 (remainder n 4))
           (mean (list (item (- (floor (/ n 4)) 1) sort-set) (item (floor (/ n 4)) sort-set))))
          (else
           (item (floor (/ n 4)) sort-set)))))

(define (q1 data-set)
  (q-med (sort data-set)))

(define (q3 data-set)
  (q-med (reverse (sort data-set))))

(define (5-num-summary data-set)
  (list (minimum data-set) (q1 data-set) (median data-set) (q3 data-set) (maximum data-set)))

(define (analyze-data data-set)
  (display "5 number summary: {") (display (car (5-num-summary data-set))) (display ", ")
  (display (cadr (5-num-summary data-set))) (display ", ")
  (display (caddr (5-num-summary data-set))) (display ", ")
  (display         (cadddr (5-num-summary data-set)))(display ", ")
  (display   (car (cddddr (5-num-summary data-set)))) (display "}")
  (newline)
  (display "Mean: ") (display (mean data-set))
  (newline)
  (display "Standard deviation: ") (display (standard-dev data-set)))