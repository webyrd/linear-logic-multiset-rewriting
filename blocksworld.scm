(load "mk/mk.scm")
(load "llmr.scm")

(define rules
  (lambda (p q name)
    (conde
      [(fresh (block surface)
          (== '((on ,block ,surface) (clear ,block) (free)) p)
          (== '((holding ,block) (clear ,surface)) q)
          (== 'pickup name))]
      [(fresh (block surface)
          (== '((holding ,block) (clear ,surface)) q)
          (== '((on ,block ,surface) (clear ,block) (free)) p)
          (== 'putdown name))]
     )))

(define initial-state '((on a table) (on b a) (on c b) (clear c) (clear table) (free)))

;;(define goal-state '((on a b) (on b c) (clear a)))
(define goal-state '((holding c)))

#!eof


(run 1 (tr)
  (fresh (final-state trace remainder-state)
    (== `(,initial-state . ,trace) tr)
    (split goal-state final-state remainder-state)
    (step* initial-state final-state trace)))

