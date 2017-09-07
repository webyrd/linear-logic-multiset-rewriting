(load "mk/mk.scm")


(define initial-state '(fox hunger fox rabbit rabbit))

;; (define split
;;   (lambda (p delta delta^)
;;     (conde
;;       [(== '() p) (== delta delta^)]
;;       [(== '() delta) (== '() p) (== '() delta^)]
;;       [(fresh (x rest res)
;;          (== `(,x . ,rest) delta)
;;          (rembero x p res)
;;          (split res rest delta^))])))

(define appendo
  (lambda (l s ls)
    (conde
      [(== '() l) (== s ls)]
      [(fresh (x rest rest+s)
         (== `(,x . ,rest) l)
         (== `(,x . ,rest+s) ls)
         (appendo rest s rest+s))])))

(define rembero
  (lambda (x l out)
    (fresh (y rest)
      (== `(,y . ,rest) l)
      (conde
        [(== x y) (== rest out)]
        [(fresh (res)
           (=/= x y)
           (== `(,y . ,res) out)
           (rembero x rest res))]))))

(define split
  (lambda (p delta delta^)
    (conde
      [(== '() p) (== delta delta^)]
      [(fresh (x rest del-x)
         (== `(,x . ,rest) p)
         (rembero x delta del-x)
         (split rest del-x delta^))])))

;; (define rules
;;   (lambda (p q name)
;;     (conde
;;       [(== '(a b) p) (== '(b b) q) (== 'rule-2 name)]
;;       [(== '(a) p) (== '(a a) q) (== 'rule-1 name)])))

(define rules
  (lambda (p q name)
    (conde
      [(== '(fox rabbit hunger) p)
       (== '(fox) q)
       (== 'omnomnom name)]
      [(== '(rabbit rabbit) p)
       (== '(rabbit rabbit rabbit rabbit) q)
       (== 'rabbit-multiply name)]
      [(== '(fox fox) p)
       (== '(fox fox fox hunger) q)
       (== 'fox-multiply name)]
      [(== '(fox hunger) p)
       (== '() q)
       (== 'fox-die name)]
      [(== '(fox) p)
       (== '(fox hunger) q)
       (== 'get-hungry name)])))

(define step
  (lambda (delta delta^^ name)
    (fresh (p q delta^)
      (split p delta delta^)
      (rules p q name)
      (appendo q delta^ delta^^))))

(define step*
  (lambda (delta delta^^ trace)
    (conde
      [(== delta delta^^) (== '() trace)]
      [(fresh (delta^ name tr)
         (== `((,name ,delta^) . ,tr) trace)
         (step delta delta^ name)
         (step* delta^ delta^^ tr))])))

#!eof

> (run 100 (q tr)
    (step* '(fox hunger fox rabbit rabbit) q tr))
((((fox hunger fox rabbit rabbit) ()))
 (((fox hunger hunger fox rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit)))))
 (((fox rabbit rabbit) ((fox-die (fox rabbit rabbit)))))
 (((fox hunger hunger hunger fox rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit)))))
 (((hunger fox rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-die (hunger fox rabbit rabbit)))))
 (((fox hunger rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (get-hungry (fox hunger rabbit rabbit)))))
 (((fox hunger hunger hunger hunger fox rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit)))))
 (((fox fox fox hunger hunger rabbit rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit)))))
 (((hunger hunger fox rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger fox rabbit rabbit)))))
 (((fox hunger hunger rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (get-hungry (fox hunger rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit)))))
 (((fox hunger hunger hunger hunger hunger fox rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger fox rabbit
             rabbit)))))
 (((fox hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-die (hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit)))))
 (((fox hunger fox fox hunger hunger rabbit rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger rabbit rabbit)))))
 (((fox fox fox hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger rabbit rabbit)))))
 (((rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (get-hungry (fox hunger rabbit rabbit))
      (fox-die (rabbit rabbit)))))
 (((hunger hunger hunger fox rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger hunger fox rabbit rabbit)))))
 (((rabbit rabbit rabbit rabbit fox)
    ((fox-die (fox rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit fox)))))
 (((fox hunger hunger hunger rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (get-hungry (fox hunger rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit)))))
 (((fox hunger hunger hunger hunger hunger hunger fox rabbit
        rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger hunger fox rabbit
             rabbit)))))
 (((rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-die (hunger fox rabbit rabbit))
      (fox-die (rabbit rabbit)))))
 (((fox hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-die (hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit)))))
 (((fox hunger hunger fox fox hunger hunger rabbit rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger fox fox hunger hunger rabbit rabbit)))))
 (((fox fox fox hunger fox hunger hunger rabbit rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger fox hunger hunger rabbit rabbit)))))
 (((fox hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit)))))
 (((fox hunger fox fox hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger hunger rabbit rabbit)))))
 (((rabbit rabbit rabbit rabbit rabbit rabbit fox)
    ((fox-die (fox rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit fox))
      (rabbit-multiply
        (rabbit rabbit rabbit rabbit rabbit rabbit fox)))))
 (((hunger rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (get-hungry (fox hunger rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit))
      (fox-die (hunger rabbit rabbit)))))
 (((hunger hunger hunger hunger fox rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger hunger hunger fox rabbit rabbit)))))
 (((fox fox rabbit) ((omnomnom (fox fox rabbit)))))
 (((hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-die (hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit))
      (fox-die (hunger rabbit rabbit)))))
 (((fox fox hunger hunger rabbit rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger rabbit rabbit))
      (fox-die (fox fox hunger hunger rabbit rabbit)))))
 (((fox fox fox hunger hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger hunger rabbit rabbit)))))
 (((fox hunger rabbit rabbit rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit fox))
      (get-hungry (fox hunger rabbit rabbit rabbit rabbit)))))
 (((fox hunger hunger hunger hunger rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (get-hungry (fox hunger rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger rabbit rabbit)))))
 (((fox hunger fox rabbit)
    ((omnomnom (fox fox rabbit))
      (get-hungry (fox hunger fox rabbit)))))
 (((fox hunger hunger hunger hunger hunger hunger hunger fox
        rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit)) (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger hunger fox rabbit
             rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger hunger hunger fox
             rabbit rabbit)))))
 (((fox hunger hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-die (hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger rabbit rabbit)))))
 (((fox hunger hunger hunger fox fox hunger hunger rabbit
        rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger fox fox hunger hunger rabbit
             rabbit)))))
 (((fox hunger fox fox hunger fox hunger hunger rabbit
        rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger fox hunger hunger rabbit
             rabbit)))))
 (((hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger fox rabbit rabbit))
      (fox-die (hunger rabbit rabbit)))))
 (((rabbit rabbit rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (get-hungry (fox hunger rabbit rabbit))
      (fox-die (rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit)))))
 (((fox fox hunger rabbit rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (fox-die (fox fox hunger rabbit rabbit)))))
 (((fox hunger hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger rabbit rabbit)))))
 (((fox hunger hunger fox fox hunger hunger hunger rabbit
        rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger fox fox hunger hunger hunger rabbit
             rabbit)))))
 (((rabbit rabbit rabbit rabbit rabbit rabbit rabbit rabbit
     fox)
    ((fox-die (fox rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit fox))
      (rabbit-multiply
        (rabbit rabbit rabbit rabbit rabbit rabbit fox))
      (rabbit-multiply
        (rabbit rabbit rabbit rabbit rabbit rabbit rabbit rabbit
          fox)))))
 (((fox fox fox hunger fox hunger hunger hunger rabbit
        rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger fox hunger hunger hunger rabbit
             rabbit)))))
 (((rabbit rabbit rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-die (hunger fox rabbit rabbit))
      (fox-die (rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit)))))
 (((fox hunger hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger rabbit rabbit)))))
 (((fox hunger fox fox hunger hunger hunger hunger rabbit
        rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger hunger hunger rabbit
             rabbit)))))
 (((fox hunger hunger rabbit rabbit rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit fox))
      (get-hungry (fox hunger rabbit rabbit rabbit rabbit))
      (get-hungry
        (fox hunger hunger rabbit rabbit rabbit rabbit)))))
 (((hunger hunger rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (get-hungry (fox hunger rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit))
      (fox-die (hunger hunger rabbit rabbit)))))
 (((hunger hunger hunger hunger hunger fox rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit)) (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger hunger fox rabbit
             rabbit))
      (fox-die
        (hunger hunger hunger hunger hunger fox rabbit rabbit)))))
 (((hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-die (hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit))
      (fox-die (hunger hunger rabbit rabbit)))))
 (((fox hunger hunger fox rabbit)
    ((omnomnom (fox fox rabbit))
      (get-hungry (fox hunger fox rabbit))
      (get-hungry (fox hunger hunger fox rabbit)))))
 (((hunger fox fox hunger hunger rabbit rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger fox fox hunger hunger rabbit rabbit))
      (fox-die (hunger fox fox hunger hunger rabbit rabbit)))))
 (((fox hunger fox rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (omnomnom (fox hunger fox rabbit)))))
 (((fox fox fox hunger rabbit)
    ((omnomnom (fox fox rabbit))
      (fox-multiply (fox fox fox hunger rabbit)))))
 (((hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit))
      (fox-die (hunger hunger rabbit rabbit)))))
 (((fox fox hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger hunger rabbit rabbit))
      (fox-die (fox fox hunger hunger hunger rabbit rabbit)))))
 (((fox rabbit)
    ((fox-die (fox rabbit rabbit))
      (get-hungry (fox hunger rabbit rabbit))
      (omnomnom (fox rabbit)))))
 (((fox hunger fox hunger hunger rabbit rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger rabbit rabbit))
      (fox-die (fox fox hunger hunger rabbit rabbit))
      (get-hungry (fox hunger fox hunger hunger rabbit rabbit)))))
 (((fox hunger rabbit rabbit rabbit rabbit rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit fox))
      (rabbit-multiply
        (rabbit rabbit rabbit rabbit rabbit rabbit fox))
      (get-hungry
        (fox hunger rabbit rabbit rabbit rabbit rabbit rabbit)))))
 (((rabbit rabbit rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit fox))
      (get-hungry (fox hunger rabbit rabbit rabbit rabbit))
      (fox-die (rabbit rabbit rabbit rabbit)))))
 (((rabbit rabbit rabbit rabbit fox hunger fox)
    ((rabbit-multiply
       (rabbit rabbit rabbit rabbit fox hunger fox)))))
 (((fox fox fox hunger hunger hunger hunger hunger rabbit
        rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger hunger hunger rabbit
             rabbit)))))
 (((fox rabbit)
    ((omnomnom (fox fox rabbit))
      (get-hungry (fox hunger fox rabbit))
      (fox-die (fox rabbit)))))
 (((fox hunger hunger hunger hunger hunger rabbit rabbit)
    ((fox-die (fox rabbit rabbit)) (get-hungry (fox hunger rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger rabbit rabbit)))))
 (((rabbit rabbit rabbit rabbit rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (get-hungry (fox hunger rabbit rabbit))
      (fox-die (rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit))
      (rabbit-multiply
        (rabbit rabbit rabbit rabbit rabbit rabbit)))))
 (((fox hunger hunger hunger hunger hunger hunger hunger
        hunger fox rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit)) (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger hunger fox rabbit
             rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger hunger hunger fox
             rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger hunger hunger hunger
             fox rabbit rabbit)))))
 (((fox hunger hunger hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit)) (fox-die (hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger rabbit rabbit)))))
 (((fox hunger hunger hunger hunger fox fox hunger hunger
        rabbit rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger fox fox hunger hunger rabbit
             rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox fox hunger hunger
             rabbit rabbit)))))
 (((rabbit rabbit rabbit rabbit rabbit rabbit rabbit rabbit
     rabbit rabbit fox)
    ((fox-die (fox rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit fox))
      (rabbit-multiply
        (rabbit rabbit rabbit rabbit rabbit rabbit fox))
      (rabbit-multiply
        (rabbit rabbit rabbit rabbit rabbit rabbit rabbit rabbit
          fox))
      (rabbit-multiply
        (rabbit rabbit rabbit rabbit rabbit rabbit rabbit rabbit
          rabbit rabbit fox)))))
 (((fox hunger hunger fox rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (omnomnom (fox hunger fox rabbit))
      (get-hungry (fox hunger hunger fox rabbit)))))
 (((fox hunger hunger fox fox hunger fox hunger hunger rabbit
        rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger fox fox hunger fox hunger hunger rabbit
             rabbit)))))
 (((fox hunger fox hunger rabbit rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (fox-die (fox fox hunger rabbit rabbit))
      (get-hungry (fox hunger fox hunger rabbit rabbit)))))
 (((fox hunger hunger hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit)) (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger rabbit rabbit)))))
 (((fox hunger hunger hunger fox fox hunger hunger hunger
        rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger fox fox hunger hunger hunger rabbit
             rabbit))
      (get-hungry
        (fox hunger hunger hunger fox fox hunger hunger hunger
             rabbit rabbit)))))
 (((rabbit rabbit rabbit rabbit rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-die (hunger fox rabbit rabbit))
      (fox-die (rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit))
      (rabbit-multiply
        (rabbit rabbit rabbit rabbit rabbit rabbit)))))
 (((fox hunger fox fox hunger fox hunger hunger hunger rabbit
        rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger fox hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger fox hunger hunger hunger rabbit
             rabbit)))))
 (((fox fox fox hunger fox hunger fox hunger hunger rabbit
        rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger fox hunger hunger rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger fox hunger fox hunger hunger rabbit
             rabbit)))))
 (((fox fox hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger rabbit rabbit))
      (fox-die (fox fox hunger hunger rabbit rabbit)))))
 (((hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger rabbit rabbit)))))
 (((fox hunger hunger hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit)) (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger rabbit rabbit)))))
 (((fox fox fox hunger hunger fox hunger hunger rabbit
        rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger fox hunger hunger rabbit
             rabbit)))))
 (((fox hunger hunger fox fox hunger hunger hunger hunger
        rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger hunger hunger rabbit
             rabbit))
      (get-hungry
        (fox hunger hunger fox fox hunger hunger hunger hunger
             rabbit rabbit)))))
 (((fox hunger hunger hunger rabbit rabbit rabbit rabbit)
    ((fox-die (fox rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit fox))
      (get-hungry (fox hunger rabbit rabbit rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger rabbit rabbit rabbit rabbit)))))
 (((fox fox fox hunger fox hunger hunger hunger hunger rabbit
        rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger hunger rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger fox hunger hunger hunger hunger rabbit
             rabbit)))))
 (((fox hunger hunger hunger fox rabbit)
    ((omnomnom (fox fox rabbit))
      (get-hungry (fox hunger fox rabbit))
      (get-hungry (fox hunger hunger fox rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit)))))
 (((fox rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (omnomnom (fox hunger fox rabbit))
      (fox-die (fox rabbit)))))
 (((fox hunger fox fox hunger rabbit)
    ((omnomnom (fox fox rabbit))
      (fox-multiply (fox fox fox hunger rabbit))
      (get-hungry (fox hunger fox fox hunger rabbit)))))
 (((fox hunger rabbit)
    ((fox-die (fox rabbit rabbit))
      (get-hungry (fox hunger rabbit rabbit))
      (omnomnom (fox rabbit))
      (get-hungry (fox hunger rabbit)))))
 (((fox hunger hunger rabbit rabbit rabbit rabbit rabbit
        rabbit)
    ((fox-die (fox rabbit rabbit))
      (rabbit-multiply (rabbit rabbit rabbit rabbit fox))
      (rabbit-multiply
        (rabbit rabbit rabbit rabbit rabbit rabbit fox))
      (get-hungry
        (fox hunger rabbit rabbit rabbit rabbit rabbit rabbit))
      (get-hungry
        (fox hunger hunger rabbit rabbit rabbit rabbit rabbit
             rabbit)))))
 (((rabbit rabbit rabbit rabbit rabbit rabbit fox hunger fox)
    ((rabbit-multiply
       (rabbit rabbit rabbit rabbit fox hunger fox))
      (rabbit-multiply
        (rabbit rabbit rabbit rabbit rabbit rabbit fox hunger
          fox)))))
 (((fox hunger hunger hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit)) (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger fox rabbit rabbit))
      (fox-die (hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger rabbit rabbit)))))
 (((fox rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (fox-die (hunger fox rabbit rabbit))
      (omnomnom (fox rabbit)))))
 (((fox hunger fox fox hunger hunger hunger hunger hunger
        rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (fox-multiply
        (fox fox fox hunger hunger hunger hunger hunger rabbit
             rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger hunger hunger hunger
  >           rabbit rabbit)))))
 (((hunger hunger hunger rabbit rabbit)
    ((fox-die (fox rabbit rabbit)) (get-hungry (fox hunger rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger hunger rabbit rabbit))
      (fox-die (hunger hunger hunger rabbit rabbit)))))
 (((hunger hunger hunger hunger hunger hunger fox rabbit
     rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit)) (get-hungry (fox hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger fox rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger hunger fox rabbit
             rabbit))
      (get-hungry
        (fox hunger hunger hunger hunger hunger hunger hunger fox
             rabbit rabbit))
      (fox-die
        (hunger hunger hunger hunger hunger hunger fox rabbit
          rabbit)))))
 (((hunger hunger hunger rabbit rabbit)
    ((get-hungry (fox hunger hunger fox rabbit rabbit)) (fox-die (hunger fox rabbit rabbit))
      (get-hungry (fox hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger rabbit rabbit))
      (get-hungry (fox hunger hunger hunger hunger rabbit rabbit))
      (fox-die (hunger hunger hunger rabbit rabbit)))))
 (((hunger hunger fox fox hunger hunger rabbit rabbit)
    ((fox-multiply (fox fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger fox fox hunger hunger rabbit rabbit))
      (get-hungry
        (fox hunger hunger hunger fox fox hunger hunger rabbit
             rabbit))
      (fox-die
        (hunger hunger fox fox hunger hunger rabbit rabbit))))))
