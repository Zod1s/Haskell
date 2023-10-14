(define (A m n)
    (cond
        ((= m 0) (+ n 1))
        ((= n 0) (+ m 1))
        (else (A (- m 1) (A m (- n 1))))))
