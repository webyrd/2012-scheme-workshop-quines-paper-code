(load "testcheck.scm")
(load "quinec.scm")

(test-check "intro-1"
  (equal? (eval quinec) quinec)
  #t)

;;; footnote moved to extending-interpreter.scm
