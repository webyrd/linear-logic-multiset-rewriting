# linear-logic-multiset-rewriting

A sexy title for a sexy topic!

Code by Chris Martens, Ramana Kumar, and Will Byrd.

A miniKanren impementation of a fragment of linear logic programming
corresponding to multiset rewriting!

Inspired by the work of Chris Martens on 'Ceptre: A Language for Modeling Generative Interactive Systems'.

A multiset rewriting program consists of:
- A set of terms/predicates that may exist in the state.
- An initial state, which is a multiset of term/predicate instances.
- A set of rules of the form P -o Q, where P and Q are multisets.

The program executes via a transition relation on states. This relation is
induced by the rules. For each rule r : P -o Q, any multiset where P is a
subset, written {M, P}, can take a step: {M, P} --> {M, Q}.

In our implementation this relation is described with the `step` relation.
`step*` is the transitive closure.


## Example: a predator-prey ecology

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
