(load "mk/mk.scm")
(load "llmr.scm")

;; Predicates:
;; (alive C)
;; (dead C)
;; (has C O)
;; (wants C O)
;; (eros C1 C2 Mag)
;; (philia C1 C2 Mag)
;; (married C1 C2)


;; Persistent - order of pos, neg, neutral
(define sentiment-order
  (lambda (s1 s2)
    (conde
      [(== neg s1) (== neutral s2)]
      [(== neutral s1) (== pos s2)])))

(define rules
  (lambda (p q name)
    (conde
      ;; fall in love
      [(fresh (char1 char2)
          (== `(fall-in-love char1 char2) name)
          (== `((eros ,char1 ,char2 pos) (philia ,char1 ,char2 neutral)) p)
          (== `((eros ,char1 ,char2 pos) (philia ,char1 ,char2 pos) q))
       )]
      ;; get crush
      [(fresh (char1 char2)
          (== `(get-crush char1 char2) name)
          (== `((philia ,char1 ,char2 pos) (eros ,char1 ,char2 neutral)) p)
          (== `((philia ,char1 ,char2 pos) (eros ,char1 ,char2 pos) q))
      )]
      ;; charm
      [(fresh (char1 char2)
          (== `(charm char1 char2) name)
          (== `((eros ,char1 ,char2 pos) (eros ,char2 ,char1 neutral)) p)
          (== `((eros ,char1 ,char2 pos) (eros ,char2 ,char1 pos)) q)
      )]
      ;; jealousy
      [(fresh (lover1 beloved lover2 sent)
          (== `(get-jealous lover1 beloved lover2) name)
          (== `((eros ,lover1 ,beloved pos) (eros ,lover2 ,beloved pos)
                (eros ,beloved ,lover2 pos) (philia ,lover1 ,lover2 ,sent))
              p)
          (== `((eros ,lover1 ,beloved pos) (eros ,lover2 ,beloved pos)
                (eros ,beloved ,lover2 pos) (philia ,lover1 ,lover2 neg)) 
              q)
      )]
      ;; insult
      ;; get married
      ;; attack
      ;; give
      ;; steal
     )))

(define initial-state 
  '((philia romeo juliet neutral) (philia romeo mercutio pos)
    (philia mercutio juliet neg) (philia mercutio romeo pos)
    (philia juliet romeo neutral) (philia juliet mercutio neutral)
    (eros romeo juliet neutral) (eros romeo mercutio neutral)
    (eros mercutio romeo neutral) (eros mercutio juliet neutral)
    (eros juliet romeo neutral) (eros juliet mercutio neutral)))

;; (define goal-state '((holding c)))
;; (define goal-state '((on c table)))
;; (define goal-state '((on a b) (on b c) (clear a)))

#!eof


;; run arbitrarily for 3 steps
(run 1 (tr)
  (fresh (final-state trace)
    (stepn 3 initial-state final-state trace)))

(run 1 (tr)
  (fresh (final-state trace remainder-state)
    (step* initial-state final-state trace)
    (split goal-state final-state remainder-state)
    (== `(,initial-state . ,trace) tr)))

(run* (rule1 state1 rule2 state2)
    (step initial-state state1 rule1)
    (step state1 state2 rule2))
