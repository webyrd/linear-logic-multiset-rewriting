(load "mk/mk.scm")

;; appendo l s ls
;; l@s = ls
(define appendo
  (lambda (l s ls)
    (conde
      [(== '() l) (== s ls)]
      [(fresh (x rest rest+s)
         (== `(,x . ,rest) l)
         (== `(,x . ,rest+s) ls)
         (appendo rest s rest+s))])))

;; "rembero x l out"
;; means x is a member of l with remainder out
;; i.e., if l = y::rest:
;; - if x=y, out=rest
;; - otherwise:
;;  * if removing x from rest yields rest-x,
;;  * return y::rest-x
(define rembero
  (lambda (x l out)
    (fresh (y rest)
      (== `(,y . ,rest) l)
      (conde
        [(== x y) (== rest out)]
        [(fresh (rest-x)
           (=/= x y)
           (== `(,y . ,rest-x) out)
           (rembero x rest rest-x))]))))

;; "split p delta delta^" means
;; delta = p + delta^
;; where + is multiset union
;; i.e.:
;; - if p is empty, split p delta = delta
;; - if p is (x+rest):
;;    * remove x from delta to get del-x
;;        (n.b. can fail if x isn't in delta)
;;    * split the rest from del-x to get delta^
(define split
  (lambda (p delta delta^)
    (conde
      [(== '() p) (== delta delta^)]
      [(fresh (x rest del-x)
         (== `(,x . ,rest) p)
         (rembero x delta del-x)
         (split rest del-x delta^))])))

;; "step delta delta^^ name" if:
;; delta steps to delta ^^ along name
;; i.e.: 
;; - there is a rule w/name "name" : p -o q
;; - delta\p is delta^
;; - delta^ + q is delta^^
(define step
  (lambda (delta delta^^ name)
    (fresh (p q delta^)
      (rules p q name)
      (split p delta delta^)
      (appendo q delta^ delta^^))))

;; reflexive, transitive closure of step
;; "step* delta delta^^ trace"
;; where trace is a list of the rules that fired
(define step*
  (lambda (delta delta^^ trace)
    (conde
      [(== delta delta^^) (== '() trace)]
      [(fresh (delta^ name tr)
         (== `((,name ,delta^) . ,tr) trace)
         (step delta delta^ name)
         (step* delta^ delta^^ tr))])))

