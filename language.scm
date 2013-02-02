(load "appendixD.scm")
(load "testcheck.scm")

(test-check "lang-1"
  (run 1 (q) (fresh (x y z) (== x z) (== 3 y)))
  '(_.0))

(test-check "lang-2"
  (run 1 (y)
    (fresh (x z) 
      (== x z)
      (== 3 y)))
  '(3))

(test-check "lang-3"
  (run 1 (q)
    (fresh (x z)
      (== x z)
      (== 3 z)
      (== q x)))
  '(3))

(test-check "lang-4"
  (run 1 (y)
    (fresh (x y)
      (== 4 x)
      (== x y))
    (== 3 y))
  '(3))

(test-check "lang-5"
  (run 1 (x) (== 4 3))
  '())

(test-check "lang-6"
  (run 1 (x) (== 5 x) (== 6 x))
  '())

(test-check "lang-7"
  (run 2 (q)
    (fresh (w x y)
      (conde
        ((== `(,x ,w ,x) q)
         (== y w))
        ((== `(,w ,x ,w) q)
         (== y w)))))
  '((_.0 _.1 _.0) (_.0 _.1 _.0)))

(test-check "lang-8"
  (run 6 (q)
    (let loop ()
      (conde
        ((== #f q))
        ((== #t q))
        ((loop)))))
  '(#f #t #f #t #f #t))

(define anyo 
  (lambda (g)
    (conde
      (g)
      ((anyo g)))))

(test-check "lang-9"
  (run 5 (q)
    (conde
      ((anyo (== #f q)))
      ((== #t q))))
  '(#t #f #f #f #f))

(test-check "lang-10"
  (run 10 (q)
    (anyo 
     (conde
       ((== 1 q))
       ((== 2 q))
       ((== 3 q)))))
  '(1 2 3 1 2 3 1 2 3 1))

(test-check "lang-11"
  (run 3 (q)
    (let ((nevero (anyo (== #f #t))))
      (conde
        ((== 1 q))
        (nevero)
        ((conde
           ((== 2 q))
           (nevero)
           ((== 3 q)))))))
  '(1 2 3))

(test-check "lang-12"
  (run* (q) (symbolo q))
  '((_.0 (sym _.0))))

(test-check "lang-13"
  (run* (q)
    (symbolo q)
    (== 4 q))
  '())

(test-check "lang-14"
  (run* (q)
    (symbolo q)
    (numbero q))
  '())

(test-check "lang-15"
  (run* (q) (numbero q))
  '((_.0 (num _.0))))

(test-check "lang-16"
  (run* (q)
    (numbero q)
    (== 4 q))
  '(4))

(test-check "lang-17"
  (run* (q)
    (numbero q)
    (numbero q))
  '((_.0 (num _.0))))

(test-check "lang-18"
  (run* (p) (=/= p 1))
  '((_.0 (=/= ((_.0 1))))))

(test-check "lang-19"
  (run* (p) (=/= 1 p) (== 1 p))
  '())

(test-check "lang-20"
  (run* (q)
    (fresh (p r)
      (=/= '(1 2) `(,p ,r))
      (== `(,p ,r) q)))
  '(((_.0 _.1) (=/= ((_.0 1) (_.1 2))))))

(test-check "lang-21"
  (run* (q)
    (fresh (p r)
      (=/= '((1) (2)) `((,p) (,r)))
      (== `(,p ,r) q)))
  '(((_.0 _.1) (=/= ((_.0 1) (_.1 2))))))

(test-check "lang-22"
  (run* (q)
    (fresh (p r)
      (=/= `((1) (,r)) `((,p) (2)))
      (== `(,p ,r) q)))
  '(((_.0 _.1) (=/= ((_.0 1) (_.1 2))))))

(test-check "lang-23"
  (run* (q)
    (fresh (p r)
      (=/= '(1 2) `(,p ,r))
      (== 1 p)
      (== `(,p ,r) q)))
  '(((1 _.0) (=/= ((_.0 2))))))

(test-check "lang-24"
  (run* (q)
    (fresh (p r)
      (=/= '(1 2) `(,p ,r))
      (== 1 p)
      (== 2 r)
      (== `(,p ,r) q)))
  '())

(test-check "lang-25"
  (run* (q)
    (fresh (p r)
      (=/= '(1 2) `(,p ,r))
      (== 1 p)
      (symbolo r)
      (== `(,p ,r) q)))
  '(((1 _.0) (sym _.0))))

(test-check "lang-26"
  (run* (q)
    (fresh (x y)
      (== `(jackal (,y leopard ,x)) q)
      (absento 'panda q)))
  '(((jackal (_.0 leopard _.1))
     (absent panda _.0)
     (absent panda _.1))))

(test-check "lang-27"
  (run* (q)
    (fresh (x y)
      (== `(jackal (,y leopard ,x)) q)
      (absento 'panda q)
      (== 'panda x)))
  '())

(test-check "lang-28"
  (run* (q)
    (fresh (x y)
      (== `(jackal (,y leopard ,x)) q)
      (absento 'panda q)
      (symbolo x)))
  '(((jackal (_.0 leopard _.1))
     (=/= ((_.1 panda)))
     (absent panda _.0)
     (sym _.1))))

(test-check "lang-29"
  (run* (q)
    (fresh (x y z)
      (== `(jackal (,y leopard ,x)) q)
      (absento 'panda q)
      (symbolo x)
      (== `(c ,z d) y)
      (== 'panda z)))
  '())
