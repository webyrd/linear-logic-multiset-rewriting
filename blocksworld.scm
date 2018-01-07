(load "mk/mk.scm")
(load "llmr.scm")

(define initial-state '((on a table) (on b a) (on c b) (clear c) (clear table) (free)))

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
