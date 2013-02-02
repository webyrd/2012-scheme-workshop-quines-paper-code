(load "appendixC.scm")
(load "appendixB.scm")
(load "interp-helpers.scm")

(define eval-expo
  (lambda (exp env val)
    (conde
      ((fresh (v)
         (== `(quote ,v) exp)
         (not-in-envo 'quote env)
         (absento 'closure v)
         (absento 'int-val v)
         (== v val)))
      ((fresh (a*)
         (== `(list . ,a*) exp)
         (not-in-envo 'list env)
         (absento 'closure a*)
         (absento 'int-val a*)
         (proper-listo a* env val)))
      ((prim-expo exp env val))
      ((symbolo exp) (lookupo exp env val))
      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) exp)
         (eval-expo rator env `(closure ,x* ,body ,env^))
         (proper-listo rands env a*)
         (ext-env*o x* a* env^ res)
         (eval-expo body res val)))
      ((fresh (x* body)
         (== `(lambda ,x* ,body) exp)
         (not-in-envo 'lambda env)
         (== `(closure ,x* ,body ,env) val))))))

(define ext-env*o
  (lambda (x* a* env out)
    (conde
      ((== '() x*) (== '() a*) (== env out))
      ((fresh (x a dx* da* env2)
         (== `(,x . ,dx*) x*)
         (== `(,a . ,da*) a*)
         (== `((,x . ,a) . ,env) env2)
         (ext-env*o dx* da* env2 out))))))

(define prim-expo
  (lambda (exp env val)
    (conde
      ((boolean-primo exp env val))
      ((number-primo exp env val))
      ((sub1-primo exp env val))
      ((zero?-primo exp env val))
      ((*-primo exp env val))    
      ((cons-primo exp env val))
      ((car-primo exp env val))
      ((cdr-primo exp env val))
      ((not-primo exp env val))
      ((if-primo exp env val)))))

(define boolean-primo
  (lambda (exp env val)
    (conde
      ((== #t exp) (== #t val))
      ((== #f exp) (== #f val)))))

(define cons-primo
  (lambda (exp env val)
    (fresh (a d v-a v-d)
      (== `(cons ,a ,d) exp)
      (== `(,v-a . ,v-d) val)
      (absento 'closure val)
      (absento 'int-val val)
      (not-in-envo 'cons env)
      (eval-expo a env v-a)
      (eval-expo d env v-d))))

(define car-primo
  (lambda (exp env val)
    (fresh (p d)
      (== `(car ,p) exp)
      (=/= 'int-val val)
      (=/= 'closure val)
      (not-in-envo 'car env)
      (eval-expo p env `(,val . ,d)))))

(define cdr-primo
  (lambda (exp env val)
    (fresh (p a)
      (== `(cdr ,p) exp)
      (=/= 'int-val a)
      (=/= 'closure a)
      (not-in-envo 'cdr env)
      (eval-expo p env `(,a . ,val)))))

(define not-primo
  (lambda (exp env val)
    (fresh (e b)
      (== `(not ,e) exp)
      (conde
        ((== #t b) (== #f val))
        ((== #f b) (== #t val)))         
      (not-in-envo 'not env)
      (eval-expo e env b))))

(define number-primo
  (lambda (exp env val)
    (fresh (n)
      (== `(int-exp ,n) exp)
      (== `(int-val ,n) val)
      (not-in-envo 'int-exp env))))

(define sub1-primo
  (lambda (exp env val)
    (fresh (e n n-1)
      (== `(sub1 ,e) exp)
      (== `(int-val ,n-1) val)
      (not-in-envo 'sub1 env)
      (eval-expo e env `(int-val ,n))
      (minuso n '(1) n-1))))

(define zero?-primo
  (lambda (exp env val)
    (fresh (e n)
      (== `(zero? ,e) exp)
      (conde
        ((zeroo n) (== #t val))
        ((poso n) (== #f val)))
      (not-in-envo 'zero? env)
      (eval-expo e env `(int-val ,n)))))

(define *-primo
  (lambda (exp env val)
    (fresh (e1 e2 n1 n2 n3)
      (== `(* ,e1 ,e2) exp)
      (== `(int-val ,n3) val)
      (not-in-envo '* env)
      (eval-expo e1 env `(int-val ,n1))
      (eval-expo e2 env `(int-val ,n2))
      (*o n1 n2 n3))))

(define if-primo
  (lambda (exp env val)
    (fresh (e1 e2 e3 t)
      (== `(if ,e1 ,e2 ,e3) exp)
      (not-in-envo 'if env)
      (eval-expo e1 env t)
      (conde
        ((== #t t) (eval-expo e2 env val))
        ((== #f t) (eval-expo e3 env val))))))

(test-check "appA-1"
  (run 12 (q) (eval-expo q '() `(int-val ,(build-num 6))))
  '((int-exp (0 1 1)) ((lambda () (int-exp (0 1 1)))) (sub1 (int-exp (1 1 1)))
  (((lambda (_.0) (int-exp (0 1 1))) '_.1)
    (=/= ((_.0 int-exp)))
    (absent closure _.1)
    (absent int-val _.1))
  (* (int-exp (1)) (int-exp (0 1 1))) (* (int-exp (0 1 1)) (int-exp (1)))
  (* (int-exp (0 1)) (int-exp (1 1)))
  (((lambda (_.0) (int-exp (0 1 1))) (list)) (=/= ((_.0 int-exp))))
  (car (list (int-exp (0 1 1))))
  ((lambda () ((lambda () (int-exp (0 1 1))))))
  (sub1 ((lambda () (int-exp (1 1 1)))))
  ((lambda () (sub1 (int-exp (1 1 1))))))) 

(test-check "appA-2"
  (car (reverse (run 7 (q) (eval-expo q '() `(int-val ,(build-num 6))))))
  '(* (int-exp (0 1)) (int-exp (1 1))))

(test-check "appA-3"
  (length (run 500 (q) (eval-expo q '() `(int-val ,(build-num 6)))))
  500)

(test-check "appA-4"
  (car (reverse (run 270 (q) (eval-expo q '() `(int-val ,(build-num 6))))))
  '(sub1 (sub1 (sub1 (int-exp (1 0 0 1))))))

(define rel-fact5
  `((lambda (f)
      ((f f) (int-exp ,(build-num 5))))
    (lambda (f)
      (lambda (n)
        (if (zero? n)
            (int-exp ,(build-num 1))
            (* n ((f f) (sub1 n))))))))

(test-check "appA-5"
  (run* (q) (eval-expo rel-fact5 '() q))
  '((int-val (0 0 0 1 1 1 1))))

(test-check "appA-6"
  (let ((ans (run 5 (q) (eval-expo q '() q))))
    (and
     (equal? ans
             '(#t
               #f
               (((lambda (_.0) (list _.0 (list 'quote _.0)))
                 '(lambda (_.0) (list _.0 (list 'quote _.0))))
                (=/= ((_.0 closure))
                     ((_.0 int-val))
                     ((_.0 list))
                     ((_.0 quote)))
                (sym _.0))
               (((lambda (_.0) (list _.0 (list (car '(quote . _.1)) _.0)))
                 '(lambda (_.0) (list _.0 (list (car '(quote . _.1)) _.0))))
                (=/= ((_.0 car))
                     ((_.0 closure))
                     ((_.0 int-val))
                     ((_.0 list))
                     ((_.0 quote)))
                (absent closure _.1)
                (absent int-val _.1)
                (sym _.0))
               (((lambda (_.0) (list (list 'lambda '(_.0) _.0) (list 'quote _.0)))
                 '(list (list 'lambda '(_.0) _.0) (list 'quote _.0)))
                (=/= ((_.0 closure))
                     ((_.0 int-val))
                     ((_.0 list))
                     ((_.0 quote)))
                (sym _.0))))
     (andmap
      (lambda (quine) (equal? (eval quine) quine))
      (map car (cddr ans)))))
  #t)
