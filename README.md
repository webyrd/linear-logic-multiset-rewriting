# linear-logic-multiset-rewriting

A sexy title for a sexy topic!

Code by Chris Martens, Ramana Kumar, and Will Byrd.

Linear-logic multi-set term rewriting in miniKanren for storytelling!

Inspired by the work of Chris Martens on 'Ceptre: A Language for Modeling Generative Interactive Systems'.

TODO: Chris, please explain what is going onnnnnnnn!  :)


A few of the rules for foxes and rabbits

```
[(fox hungry)] -> []                      :fox-die

[((fox hungry) rabbit)] -> [(fox sated)]  :omnomnom

[(fox sated)] -> [(fox hungry)]           :get-hungry
```

Two fox stories, involving two hoooongry foxes and two rabbits.  We require that at the end of the story there are at least three sated foxes and at least two rabbits:

```
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
```
