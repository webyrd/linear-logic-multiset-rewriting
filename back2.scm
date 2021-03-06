(load "mk/mk.scm")


(define initial-state '(a b))

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

(define rules
  (lambda (p q name)
    (conde
      [(== '(a b) p) (== '(b b) q) (== 'rule-2 name)]
      [(== '(a) p) (== '(a a) q) (== 'rule-1 name)])))

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

> (load "llmr.scm")
> (run 1 (q tr)
    (step* initial-state q tr))
((((a b) ())))
> (run 100 (q tr)
    (step* initial-state q tr))
((((a b) ())) (((a a b) ((rule-1 (a a b)))))
 (((b b) ((rule-2 (b b)))))
 (((a a a b) ((rule-1 (a a b)) (rule-1 (a a a b)))))
 (((a a a a b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b)))))
 (((b b a) ((rule-1 (a a b)) (rule-2 (b b a)))))
 (((a a a a a b)
    ((rule-1 (a a b))
      (rule-1 (a a a b))
      (rule-1 (a a a a b))
      (rule-1 (a a a a a b)))))
 (((a a b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)))))
 (((b b a a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)))))
 (((b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-2 (b b b)))))
 (((a a a b b)
    ((rule-1 (a a b))
      (rule-2 (b b a))
      (rule-1 (a a b b))
      (rule-1 (a a a b b)))))
 (((a a a a a a b)
    ((rule-1 (a a b))
      (rule-1 (a a a b))
      (rule-1 (a a a a b))
      (rule-1 (a a a a a b))
      (rule-1 (a a a a a a b)))))
 (((a a a a b b)
    ((rule-1 (a a b))
      (rule-2 (b b a))
      (rule-1 (a a b b))
      (rule-1 (a a a b b))
      (rule-1 (a a a a b b)))))
 (((b b a b)
    ((rule-1 (a a b))
      (rule-2 (b b a))
      (rule-1 (a a b b))
      (rule-2 (b b a b)))))
 (((a a b b a)
    ((rule-1 (a a b))
      (rule-1 (a a a b))
      (rule-2 (b b a a))
      (rule-1 (a a b b a)))))
 (((b b a a a)
    ((rule-1 (a a b))
      (rule-1 (a a a b))
      (rule-1 (a a a a b))
      (rule-2 (b b a a a)))))
 (((a a a a a b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-1 (a a a a b b)) (rule-1 (a a a a a b b)))))
 (((a a a a a a a b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-1 (a a a a a b)) (rule-1 (a a a a a a b))
      (rule-1 (a a a a a a a b)))))
 (((a a a b b a)
    ((rule-1 (a a b))
      (rule-1 (a a a b))
      (rule-2 (b b a a))
      (rule-1 (a a b b a))
      (rule-1 (a a a b b a)))))
 (((b b b a)
    ((rule-1 (a a b))
      (rule-1 (a a a b))
      (rule-2 (b b a a))
      (rule-2 (b b b a)))))
 (((a a b b b)
    ((rule-1 (a a b))
      (rule-2 (b b a))
      (rule-1 (a a b b))
      (rule-2 (b b a b))
      (rule-1 (a a b b b)))))
 (((b b a a b)
    ((rule-1 (a a b))
      (rule-2 (b b a))
      (rule-1 (a a b b))
      (rule-1 (a a a b b))
      (rule-2 (b b a a b)))))
 (((a a a a a a b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-1 (a a a a b b)) (rule-1 (a a a a a b b))
      (rule-1 (a a a a a a b b)))))
 (((a a a a b b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-1 (a a b b a))
      (rule-1 (a a a b b a)) (rule-1 (a a a a b b a)))))
 (((a a a b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b)))))
 (((a a b b a a)
    ((rule-1 (a a b))
      (rule-1 (a a a b))
      (rule-1 (a a a a b))
      (rule-2 (b b a a a))
      (rule-1 (a a b b a a)))))
 (((b b b b)
    ((rule-1 (a a b))
      (rule-2 (b b a))
      (rule-1 (a a b b))
      (rule-2 (b b a b))
      (rule-2 (b b b b)))))
 (((b b a b a)
    ((rule-1 (a a b))
      (rule-1 (a a a b))
      (rule-2 (b b a a))
      (rule-1 (a a b b a))
      (rule-2 (b b a b a)))))
 (((a a a a a a a a b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-1 (a a a a a b)) (rule-1 (a a a a a a b))
      (rule-1 (a a a a a a a b)) (rule-1 (a a a a a a a a b)))))
 (((b b a a a a)
    ((rule-1 (a a b))
      (rule-1 (a a a b))
      (rule-1 (a a a a b))
      (rule-1 (a a a a a b))
      (rule-2 (b b a a a a)))))
 (((a a b b a b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-2 (b b a a b)) (rule-1 (a a b b a b)))))
 (((a a b b b)
    ((rule-1 (a a b))
      (rule-1 (a a a b))
      (rule-2 (b b a a))
      (rule-2 (b b b a))
      (rule-1 (a a b b b)))))
 (((b b a a a b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-1 (a a a a b b)) (rule-2 (b b a a a b)))))
 (((a a a a b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-1 (a a a a b b b)))))
 (((a a a a a a a b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-1 (a a a a b b)) (rule-1 (a a a a a b b))
      (rule-1 (a a a a a a b b)) (rule-1 (a a a a a a a b b)))))
 (((b b a b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-2 (b b a b b)))))
 (((a a a a a b b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-1 (a a b b a))
      (rule-1 (a a a b b a)) (rule-1 (a a a a b b a))
      (rule-1 (a a a a a b b a)))))
 (((a a a b b a a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-2 (b b a a a)) (rule-1 (a a b b a a))
      (rule-1 (a a a b b a a)))))
 (((b b b a a)
    ((rule-1 (a a b))
      (rule-1 (a a a b))
      (rule-1 (a a a a b))
      (rule-2 (b b a a a))
      (rule-2 (b b b a a)))))
 (((b b b b)
    ((rule-1 (a a b))
      (rule-1 (a a a b))
      (rule-2 (b b a a))
      (rule-2 (b b b a))
      (rule-2 (b b b b)))))
 (((a a a b b a b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-2 (b b a a b)) (rule-1 (a a b b a b))
      (rule-1 (a a a b b a b)))))
 (((a a a b b b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-2 (b b b a))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b)))))
 (((a a a a a b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-1 (a a a a b b b)) (rule-1 (a a a a a b b b)))))
 (((b b b a b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-2 (b b a a b)) (rule-2 (b b b a b)))))
 (((a a b b b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-1 (a a b b a))
      (rule-2 (b b a b a)) (rule-1 (a a b b b a)))))
 (((a a b b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-2 (b b a b b))
      (rule-1 (a a b b b b)))))
 (((a a b b a a a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-1 (a a a a a b)) (rule-2 (b b a a a a))
      (rule-1 (a a b b a a a)))))
 (((b b a a b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-2 (b b a a b b)))))
 (((b b a a b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-1 (a a b b a))
      (rule-1 (a a a b b a)) (rule-2 (b b a a b a)))))
 (((a a b b a a b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-1 (a a a a b b)) (rule-2 (b b a a a b))
      (rule-1 (a a b b a a b)))))
 (((a a a a a a a a a b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-1 (a a a a a b)) (rule-1 (a a a a a a b))
      (rule-1 (a a a a a a a b)) (rule-1 (a a a a a a a a b))
      (rule-1 (a a a a a a a a a b)))))
 (((a a a a b b a a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-2 (b b a a a)) (rule-1 (a a b b a a))
      (rule-1 (a a a b b a a)) (rule-1 (a a a a b b a a)))))
 (((a a a a a a b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-1 (a a a a b b b)) (rule-1 (a a a a a b b b))
      (rule-1 (a a a a a a b b b)))))
 (((a a a a b b b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-2 (b b b a))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-1 (a a a a b b b)))))
 (((a a a a a a a a b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-1 (a a a a b b)) (rule-1 (a a a a a b b))
      (rule-1 (a a a a a a b b)) (rule-1 (a a a a a a a b b))
      (rule-1 (a a a a a a a a b b)))))
 (((b b a a a a b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-1 (a a a a b b)) (rule-1 (a a a a a b b))
      (rule-2 (b b a a a a b)))))
 (((b b a a a a a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-1 (a a a a a b)) (rule-1 (a a a a a a b))
      (rule-2 (b b a a a a a)))))
 (((b b a b a a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-2 (b b a a a)) (rule-1 (a a b b a a))
      (rule-2 (b b a b a a)))))
 (((a a a b b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-2 (b b a b b))
      (rule-1 (a a b b b b)) (rule-1 (a a a b b b b)))))
 (((a a a a a a b b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-1 (a a b b a))
      (rule-1 (a a a b b a)) (rule-1 (a a a a b b a))
      (rule-1 (a a a a a b b a)) (rule-1 (a a a a a a b b a)))))
 (((b b a b b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-2 (b b b a))
      (rule-1 (a a b b b)) (rule-2 (b b a b b)))))
 (((b b b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-2 (b b a b b))
      (rule-2 (b b b b b)))))
 (((a a a a b b a b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-2 (b b a a b)) (rule-1 (a a b b a b))
      (rule-1 (a a a b b a b)) (rule-1 (a a a a b b a b)))))
 (((a a b b b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-2 (b b a a a)) (rule-2 (b b b a a))
      (rule-1 (a a b b b a)))))
 (((a a b b a b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-2 (b b a a b b)) (rule-1 (a a b b a b b)))))
 (((a a a b b b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-1 (a a b b a))
      (rule-2 (b b a b a)) (rule-1 (a a b b b a))
      (rule-1 (a a a b b b a)))))
 (((a a a b b a a a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-1 (a a a a a b)) (rule-2 (b b a a a a))
      (rule-1 (a a b b a a a)) (rule-1 (a a a b b a a a)))))
 (((b b a a a b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-1 (a a a a b b b)) (rule-2 (b b a a a b b)))))
 (((a a a b b a a b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-1 (a a a a b b)) (rule-2 (b b a a a b))
      (rule-1 (a a b b a a b)) (rule-1 (a a a b b a a b)))))
 (((a a a a a b b b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-2 (b b b a))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-1 (a a a a b b b)) (rule-1 (a a a a a b b b)))))
 (((a a a a a b b a a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-2 (b b a a a)) (rule-1 (a a b b a a))
      (rule-1 (a a a b b a a)) (rule-1 (a a a a b b a a))
      (rule-1 (a a a a a b b a a)))))
 (((b b a b a b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-2 (b b a a b)) (rule-1 (a a b b a b))
      (rule-2 (b b a b a b)))))
 (((a a a a a a a b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-1 (a a a a b b b)) (rule-1 (a a a a a b b b))
      (rule-1 (a a a a a a b b b))
      (rule-1 (a a a a a a a b b b)))))
 (((b b b b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-1 (a a b b a))
      (rule-2 (b b a b a)) (rule-2 (b b b b a)))))
 (((b b b a a a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-1 (a a a a a b)) (rule-2 (b b a a a a))
      (rule-2 (b b b a a a)))))
 (((b b b a a b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-1 (a a a a b b)) (rule-2 (b b a a a b))
      (rule-2 (b b b a a b)))))
 (((a a b b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-2 (b b a a b)) (rule-2 (b b b a b))
      (rule-1 (a a b b b b)))))
 (((a a a a b b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-2 (b b a b b))
      (rule-1 (a a b b b b)) (rule-1 (a a a b b b b))
      (rule-1 (a a a a b b b b)))))
 (((a a b b b b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-2 (b b b a))
      (rule-1 (a a b b b)) (rule-2 (b b a b b))
      (rule-1 (a a b b b b)))))
 (((a a b b a b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-1 (a a b b a))
      (rule-1 (a a a b b a)) (rule-2 (b b a a b a))
      (rule-1 (a a b b a b a)))))
 (((b b a a b b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-2 (b b b a))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-2 (b b a a b b)))))
 (((a a a b b b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-2 (b b a a a)) (rule-2 (b b b a a))
      (rule-1 (a a b b b a)) (rule-1 (a a a b b b a)))))
 (((a a a b b a b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-2 (b b a a b b)) (rule-1 (a a b b a b b))
      (rule-1 (a a a b b a b b)))))
 (((b b b b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-2 (b b a a a)) (rule-2 (b b b a a))
      (rule-2 (b b b b a)))))
 (((a a a a a b b a b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-2 (b b a a b)) (rule-1 (a a b b a b))
      (rule-1 (a a a b b a b)) (rule-1 (a a a a b b a b))
      (rule-1 (a a a a a b b a b)))))
 (((b b a b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-2 (b b a b b))
      (rule-1 (a a b b b b)) (rule-2 (b b a b b b)))))
 (((b b a a a b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-1 (a a b b a))
      (rule-1 (a a a b b a)) (rule-1 (a a a a b b a))
      (rule-2 (b b a a a b a)))))
 (((a a b b a a a b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-1 (a a a a b b)) (rule-1 (a a a a a b b))
      (rule-2 (b b a a a a b)) (rule-1 (a a b b a a a b)))))
 (((a a b b a a a a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-1 (a a a a a b)) (rule-1 (a a a a a a b))
      (rule-2 (b b a a a a a)) (rule-1 (a a b b a a a a)))))
 (((a a b b b a a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-2 (b b a a a)) (rule-1 (a a b b a a))
      (rule-2 (b b a b a a)) (rule-1 (a a b b b a a)))))
 (((a a a a a a b b b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-2 (b b b a))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-1 (a a a a b b b)) (rule-1 (a a a a a b b b))
      (rule-1 (a a a a a a b b b)))))
 (((b b b a b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-2 (b b a b))
      (rule-1 (a a b b b)) (rule-1 (a a a b b b))
      (rule-2 (b b a a b b)) (rule-2 (b b b a b b)))))
 (((a a a a a a a a a a b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-1 (a a a a a b)) (rule-1 (a a a a a a b))
      (rule-1 (a a a a a a a b)) (rule-1 (a a a a a a a a b))
      (rule-1 (a a a a a a a a a b))
      (rule-1 (a a a a a a a a a a b)))))
 (((b b a a b a a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-1 (a a a a b))
      (rule-2 (b b a a a)) (rule-1 (a a b b a a))
      (rule-1 (a a a b b a a)) (rule-2 (b b a a b a a)))))
 (((a a a a a a a a a b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-1 (a a a a b b)) (rule-1 (a a a a a b b))
      (rule-1 (a a a a a a b b)) (rule-1 (a a a a a a a b b))
      (rule-1 (a a a a a a a a b b))
      (rule-1 (a a a a a a a a a b b)))))
 (((a a a b b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-2 (b b a a b)) (rule-2 (b b b a b))
      (rule-1 (a a b b b b)) (rule-1 (a a a b b b b)))))
 (((a a a a a a a b b a)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-1 (a a b b a))
      (rule-1 (a a a b b a)) (rule-1 (a a a a b b a))
      (rule-1 (a a a a a b b a)) (rule-1 (a a a a a a b b a))
      (rule-1 (a a a a a a a b b a)))))
 (((a a a b b b b)
    ((rule-1 (a a b)) (rule-1 (a a a b)) (rule-2 (b b a a)) (rule-2 (b b b a))
      (rule-1 (a a b b b)) (rule-2 (b b a b b))
      (rule-1 (a a b b b b)) (rule-1 (a a a b b b b)))))
 (((b b b b b)
    ((rule-1 (a a b)) (rule-2 (b b a)) (rule-1 (a a b b)) (rule-1 (a a a b b))
      (rule-2 (b b a a b)) (rule-2 (b b b a b))
      (rule-2 (b b b b b)))))
 (((a a b b a a b b)
   ((rule-1 (a a b))
    (rule-2 (b b a))
    (rule-1 (a a b b))
    (rule-2 (b b a b))
    (rule-1 (a a b b b))
    (rule-1 (a a a b b b))
    (rule-1 (a a a a b b b))
    (rule-2 (b b a a a b b))
    (rule-1 (a a b b a a b b))))))
