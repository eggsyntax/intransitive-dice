(ns intransitive-dice.core
  "https://en.wikipedia.org/wiki/Intransitive_dice"
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as com]))

(defn compare-rolls-pair
  "Given two vectors of numbers A and B, return in what percentage of combinations
  (of one from A and one from B) A > B"
  [A B]
  (let [num-pairs (* (count A) (count B))
        pairs (com/cartesian-product A B)
        win-loss-results (for [[a b] pairs] (if (> a b) 1 0))
        win-sum (apply + win-loss-results)
        win-fraction (/ (float win-sum) num-pairs)]
    ;; (prn pairs)
    ;; (prn win-loss-results)
    ;; (prn win-sum)
    ;; (prn win-fraction)
    win-fraction))

(defn compare-rolls
  "Given three dice A, B, and C, where each die is represented as a vector of side-values,
  return for each pair A:B, B:C, C:A the fraction of all possible pair-rolls where the
  value of the first die is higher than the value of the second die. Dice are assumed to
  be fair and can have an arbitrary number of sides (and can have sides with negative or
  fractional values)."
  [[A B C]]
  (let [result-scores (atom [])]
    (doall
     (for [pair [[A B] [B C] [A C]]]
       (let [x (first pair), y (second pair)
             result-percentage (compare-rolls-pair x y)]
         ;; (println pair ":")
         ;; (println (first pair))
         ;; (println (second pair))
         (println
          (format  "%s wins %.02f of the time against %s."
                   (first pair) result-percentage (second pair)))
         (println)
         ;; (println (first pair) "wins" (compare-rolls-pair x y) "of the time.")
         ;; (println)
         (swap! result-scores conj result-percentage)
         )))
    @result-scores))

(defn test-dice [dice]
  (let [[ab bc ac] (compare-rolls dice)]
    (println (format "A vs B: %.02f" ab))
    (println (format "B vs C: %.02f" bc))
    (println (format "A vs C: %.02f" ac))
    (if (and (>  ab 0.5)
             (>  bc 0.5)
             (<= ac 0.5))
      (do (println "Success!")
          (if (and (> ab 0.5)
                   (> bc 0.5)
                   (< ac 0.5))
            (println "MAD SUCCESS!!!!")))
      (println "Sorry, failure."))))


;; Working examples:
(comment
  (test-dice [[2 3 10 11]
              [2 6  8  9]
              [1 5  7 16]]) ; weak form
  )

(comment
  ;; earlier stab at macroish version that foundered on inability to do evaluation of locals in
  ;; macros. My goal was to declare the pairs as [['A 'B] '['B 'C] ['A 'C]] and use that when
  ;; printing results for each roll. If so inclined, there are various resources online that
  ;; might be helpful for pursuing it further, eg
  ;; https://www.mail-archive.com/clojure@googlegroups.com/msg98801.html
  ;; which points to the spyscope `spy` macro: https://github.com/clojure/tools.logging/blob/master/src/main/clojure/clojure/tools/logging.clj#L120
  ;; info on &env: http://blog.jayfields.com/2011/02/clojure-and.html
  ;;

  (defmacro print-with-name [x]
    (prn "env:" &env)
    (prn "form:" &form)
    (let [x# x]
      `(println ~x ~x# (type ~x) (type ~x#))))

  (defn compare-rolls
    "Given three dice A, B, and C, where each die is represented as a vector of side-values,
  return for each pair A:B, B:C, C:A the fraction of all possible pair-rolls where the
  value of the first die is higher than the value of the second die. Dice are assumed to
  be fair and can have an arbitrary number of sides (and can have sides with negative or
  fractional values)."
    [[A B C]]
    (let [result-scores (atom [])]
      (for [pair [[A B] [B C] [C A]]]
        (let [x (first pair), y (second pair)
              result-percentage (compare-rolls-pair x y)]
          (print-with-name pair)
          (println pair ":")
          (println (first pair))
          (println (second pair))
          (println
           (format  "%s wins %02f of the time against %s."
                    (first pair) result-percentage (second pair)))
          (println)
          ;; (println (first pair) "wins" (compare-rolls-pair x y) "of the time.")
          ;; (println)
          )
        ))
    )

  )
