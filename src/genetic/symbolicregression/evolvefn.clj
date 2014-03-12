;taken from https://github.com/lspector/gp/blob/3dc8a1219477a522cb9135f0b781bae3d9aa4929/src/gp/evolvefn.clj

;; Lee Spector (lspector@hampshire.edu) 20111018 - 20131115

;; 20111113 update: handles functions of different arities
;; 20120829 update: packaged into the gp project
;; 20131115 update: eliminated use of zippers

(ns genetic.symbolicregression.evolvefn)

;; This code defines and runs a genetic programming system on the problem
;; of finding a function that fits a particular set of [x y] pairs.

;; The aim here is mostly to demonstrate how genetic programming can be
;; implemented in Clojure simply and clearly, and several things are
;; done in somewhat inefficient and/or non-standard ways. But this should
;; provide a reasonable starting point for developing more efficient/
;; standard/capable systems.

;; Note also that this code, as written, will not always find a solution.
;; There are a variety of changes that one might make to improve its
;; problem-solving performance on the given problem.

;; We'll use data from x^2 + x + 1 (the problem from chapter 4 of
;; http://www.gp-field-guide.org.uk/, although our gp algorithm won't
;; be the same, and we'll use some different parameters as well).

;; We'll use input (x) values ranging from -1.0 to 1.0 in increments
;; of 0.1, and we'll generate the target [x y] pairs algorithmically.
;; If you want to evolve a function to fit your own data then you could
;; just paste a vector of pairs into the definition of target-data instead.

(def target-data
  (map #(vector % (+ (* % %) % 1))
       (range -1.0 1.0 0.1)))

;; An individual will be an expression made of functions +, -, *, and
;; pd (protected division), along with terminals x and randomly chosen
;; constants between -5.0 and 5.0. Note that for this problem the
;; presence of the constants actually makes it much harder, but that
;; may not be the case for other problems.

;; We'll the functions and the arities in a map.

(def function-table (zipmap '(+ - * (fn [num denom] (if (zero? denom) 0 (/ num denom))))
                            '(2 2 2 2 )))

(defn random-function
  []
  (rand-nth (keys function-table)))

(defn random-terminal
  []
  (rand-nth (list 'x (- (rand 10) 5))))

(defn random-code
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2))) ; might want to try (rand-int (count function-table))
    (random-terminal)
    (let [f (random-function)]
      (cons f (repeatedly (get function-table f)
                          #(random-code (dec depth)))))))

;; And we have to define pd (protected division):

(defn pd
  "Protected division; returns 0 if the denominator is zero."
  [num denom]
  (if (zero? denom)
    0
    (/ num denom)))

;; We can now evaluate the error of an individual by creating a function
;; built around the individual, calling it on all of the x values, and
;; adding up all of the differences between the results and the
;; corresponding y values.

(defn error
  [individual target-data]
  (let [value-function (eval (list 'fn '[x] individual))]
    (reduce + (map (fn [[x y]]
                     (Math/abs
                       (- (value-function x) y)))
                   target-data))))

;; We can now generate and evaluate random small programs, as with:

;; (let [i (random-code 3)] (println (error i) "from individual" i))

;; To help write mutation and crossover functions we'll write a utility
;; function that returns a random subtree from an expression and another that
;; replaces a random subtree of an expression.

(defn codesize [c]
  (if (seq? c)
    (count (flatten c))
    1))

(defn random-subtree
  [i]
  (if (zero? (rand-int (codesize i)))
    i
    (random-subtree
      (rand-nth
        (apply concat
               (map #(repeat (codesize %) %)
                    (rest i)))))))

;(random-subtree '(+ (* x (+ y z)) w))

(defn replace-random-subtree
  [i replacement]
  (if (zero? (rand-int (codesize i)))
    replacement
    (let [position-to-change
          (rand-nth
            (apply concat
                   (map #(repeat (codesize %1) %2)
                        (rest i)
                        (iterate inc 1))))]
          (map #(if %1 (replace-random-subtree %2 replacement) %2)
               (for [n (iterate inc 0)] (= n position-to-change))
               i))))

;(replace-random-subtree '(0 (1) (2 2) (3 3 3) (4 4 4 4) (5 5 5 5 5) (6 6 6 6 6 6 6)) 'x)

;(replace-random-subtree '(+ (* x (+ y z)) w) 3)

(defn mutate
  [i]
  (replace-random-subtree i (random-code 2)))

;(mutate '(+ (* x (+ y z)) w))

(defn crossover
  [i j]
  (replace-random-subtree i (random-subtree j)))

;(crossover '(+ (* x (+ y z)) w) '(/ a (/ (/ b c) d)))

; We can see some mutations with:
; (let [i (random-code 2)] (println (mutate i) "from individual" i))

; and crossovers with:
; (let [i (random-code 2) j (random-code 2)]
;   (println (crossover i j) "from" i "and" j))
