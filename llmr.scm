(load "mk/mk.scm")


(define initial-state '((fox hungry) (fox hungry) rabbit rabbit))

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

#|
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
|#

#|
(define rules
  (lambda (p q name)
    (conde
      [(== '((fox hungry) rabbit) p)
       (== '((fox sated)) q)
       (== 'omnomnom name)]
      [(== '(rabbit rabbit) p)
       (== '(rabbit rabbit rabbit rabbit) q)
       (== 'rabbit-multiply name)]
      [(fresh (h1 h2)
         (== `((fox ,h1) (fox ,h2)) p)
         (== `((fox ,h1) (fox ,h2) (fox hungry)) q)
         (== 'fox-multiply name))]
      [(== '((fox hungry)) p)
       (== '() q)
       (== 'fox-die name)]
      [(== '((fox sated)) p)
       (== '((fox hungry)) q)
       (== 'get-hungry name)])))
|#

(define rules
  (lambda (p q name)
    (conde
      [(== '((fox hungry)) p)
       (== '() q)
       (== 'fox-die name)]
      [(== '((fox hungry) rabbit) p)
       (== '((fox sated)) q)
       (== 'omnomnom name)]
      [(== '((fox sated)) p)
       (== '((fox hungry)) q)
       (== 'get-hungry name)]      
      [(fresh (h1 h2)
         (== `((fox ,h1) (fox ,h2)) p)
         (== `((fox ,h1) (fox ,h2) (fox hungry)) q)
         (== 'fox-multiply name))]
      [(== '(rabbit rabbit) p)
       (== '(rabbit rabbit rabbit rabbit) q)
       (== 'rabbit-multiply name)])))

(define step
  (lambda (delta delta^^ name)
    (fresh (p q delta^)
      (rules p q name)
      (split p delta delta^)
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

(run 2 (tr)
  (let ((initial-state '((fox hungry) (fox hungry) rabbit rabbit)))
    (fresh (final rest diff s s^ s2 s3 s4 r)
      (== `(,initial-state . ,rest) tr)
      (split `((fox sated) (fox sated) (fox sated) rabbit rabbit) final diff)
      (step* initial-state final rest))))
=>
(((((fox hungry) (fox hungry) rabbit rabbit)
   (rabbit-multiply
    (rabbit rabbit rabbit rabbit (fox hungry) (fox hungry)))
   (omnomnom ((fox sated) rabbit rabbit rabbit (fox hungry)))
   (omnomnom ((fox sated) (fox sated) rabbit rabbit))
   (rabbit-multiply
    (rabbit rabbit rabbit rabbit (fox sated) (fox sated)))
   (fox-multiply
    ((fox sated) (fox sated) (fox hungry) rabbit rabbit rabbit
     rabbit))
   (omnomnom
    ((fox sated) (fox sated) (fox sated) rabbit rabbit
     rabbit))))
 ((((fox hungry) (fox hungry) rabbit rabbit)
   (rabbit-multiply
    (rabbit rabbit rabbit rabbit (fox hungry) (fox hungry)))
   (rabbit-multiply
    (rabbit rabbit rabbit rabbit rabbit rabbit (fox hungry)
            (fox hungry)))
   (omnomnom
    ((fox sated) rabbit rabbit rabbit rabbit rabbit
     (fox hungry)))
   (omnomnom
    ((fox sated) (fox sated) rabbit rabbit rabbit rabbit))
   (fox-multiply
    ((fox sated) (fox sated) (fox hungry) rabbit rabbit rabbit
     rabbit))
   (omnomnom
    ((fox sated) (fox sated) (fox sated) rabbit rabbit
     rabbit)))))

(run 1 (tr)
  (fresh (final rest diff)
    (== `(,initial-state . ,rest) tr)
    (split '(rabbit rabbit rabbit) final diff)
    (step* initial-state final rest)))

(run 10 (tr)
  (fresh (final rest diff)
    (== `(,initial-state . ,rest) tr)
    (split final '(rabbit rabbit rabbit) diff)
    (step* initial-state final rest)))

(run 100 (tr)
  (fresh (q rest)
    (== `(,initial-state . ,rest) tr)
    (step* initial-state q rest)))
