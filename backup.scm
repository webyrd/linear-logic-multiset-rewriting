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
  (lambda (x l l-x)
    (fresh (y rest)
      (== `(,y . ,rest) l)
      (conde
        [(== x y) (== rest l-x)]
        [(=/= x y) (rembero x rest l-x)]))))

(define split
  (lambda (p delta delta^)
    (conde
      [(== '() p) (== delta delta^)]
      [(fresh (x rest del-x)
         (== `(,x . ,rest) p)
         (rembero x delta del-x)
         (split rest del-x delta^))])))

(define rules
  (lambda (p q)
    (conde
      [(== '(a) p) (== '(a a) q)]
      [(== '(a b) p) (== '(b b) q)])))

(define step
  (lambda (delta delta^^)
    (fresh (p q delta^)
      (split p delta delta^)
      (rules p q)
      (appendo q delta^ delta^^))))

(define step*
  (lambda (delta delta^^)
    (conde
      [(== delta delta^^)]
      [(fresh (delta^)
         (step delta delta^)
         (step* delta^ delta^^))])))

#!eof

(run* (q)
    (step* initial-state q))
(((a b))
 (((a a) b))
 (((b b))))

