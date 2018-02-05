(load "mk/mk.scm")
(load "llmr.scm")

(define rules
  (lambda (p q name)
    (conde
      [(fresh (block surface)
          (== `((on ,block ,surface) (clear ,block) (free)) p)
          (== `((holding ,block) (clear ,surface)) q)
          (== `(pickup ,block) name))]
      [(fresh (block surface)
          (== `((holding ,block) (clear ,surface)) p)
          (== `((on ,block ,surface) (clear ,block) (free)) q)
          (== `(putdown ,block ,surface) name))]
      [   (== '() p)
          (== '((clear table)) q)
          (== 'clear-table name)]
     )))

(define initial-state '((on a table) (on b a) (on c b) (clear c) (clear table) (free)))

;; (define goal-state '((holding c)))
;; (define goal-state '((on c table)))
(define goal-state '((on a b) (on b c) (clear a)))

#!eof


(run 1 (tr)
  (fresh (final-state trace remainder-state)
    (step* initial-state final-state trace)
    (split goal-state final-state remainder-state)
    (== `(,initial-state . ,trace) tr)))

(run* (rule1 state1 rule2 state2)
    (step initial-state state1 rule1)
    (step state1 state2 rule2))
