(load "extending-interpreter.scm")
(load "replacestar.scm")
(load "quinec.scm")

(test-check "punchline from intro"
  (equal? (replace* '((_.0 . x)) (caar (run 1 (q) (eval-expo q '() q)))) quinec)
  #t)

(test-check "quine-gen-1"
  (run 1 (q) (eval-expo q '() q))
  '((((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))))

(test-check "quine-gen-2"
  (replace* '((_.0 . x)) (car (car (run 1 (q) (eval-expo q '() q)))))
  quinec)

(test-check "quine-gen-3"
  (run 1 (x)
    (fresh (p q)
      (=/= p q)
      (eval-expo p '() q) (eval-expo q '() p)
      (== `(,p ,q) x)))
  '((('((lambda (_.0)
          (list 'quote (list _.0 (list 'quote _.0))))
        '(lambda (_.0) (list 'quote (list _.0 (list 'quote _.0)))))
      ((lambda (_.0) (list 'quote (list _.0 (list 'quote _.0))))
       '(lambda (_.0) (list 'quote (list _.0 (list 'quote _.0))))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))))

(test-check "quine-gen-4"
  (run 1 (x)
    (fresh (p q r)
      (=/= p q) (=/= q r) (=/= r p)
      (eval-expo p '() q) (eval-expo q '() r) (eval-expo r '() p)
      (== `(,p ,q ,r) x)))
  '(((''((lambda (_.0)
           (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))
         '(lambda (_.0)
            (list 'quote (list 'quote (list _.0 (list 'quote _.0))))))
      '((lambda (_.0)
          (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))
        '(lambda (_.0)
           (list 'quote (list 'quote (list _.0 (list 'quote _.0))))))
      ((lambda (_.0)
         (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))
       '(lambda (_.0)
          (list 'quote (list 'quote (list _.0 (list 'quote _.0)))))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 quote)))
     (sym _.0))))
