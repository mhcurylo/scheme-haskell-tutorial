(define (not x)
  (if x #f #t))

(define (null? obj)
  (if (eqv? obj '()) #t #f))

(define (list . objs)
  objs)

(define (id obj)
  obj)

(define (flip func)
  (lambda (a1 a2)
    (func a2 a1)))

(define (curry func arg1)
  (lambda (arg)
    (apply func (cons arg1 (list arg)))))

(define (compose f g)
  (lambda (arg)
    (f (apply g arg))))

(define zero?
  (curry = 0))

(define positive?
  (curry < 0))

(define negative?
  (curry > 0))

(define (odd? num)
  (= (mod num 2) 1))

(define (even? num)
  (= (mod num 2) 0))

(define (foldr fun acc lst)
  (if (null? lst)
      acc
      (fun (car lst) (foldr fun acc (cdr lst)))))

(define (foldl fun acc lst)
  (if (null? lst)
      acc
      (foldl fun (fun acc (car lst)) (cdr lst))))

(define fold foldl)
(define reduce foldr)

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define (sum . lst)
  (fold + 0 lst))

(define (product . lst)
  (fold * 1 lst))

(define (and . lst)
  (fold && #t lst))

(define (or . lst)
  (fold || #f lst))

(define (max first . rest)
  (fold (lambda (old new)
          (if (new > old)
              new
              old) first rest)))

(define (min first . rest)
  (fold (lambda (old new)
          (if (new < old)
              new
              old) first rest)))

(define (length lst)
  (fold (lambda (x y) (+ x 1)) 0 lst))

(define (reverse lst)
  (fold (flip cons) '() lst))


