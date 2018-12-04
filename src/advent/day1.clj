(ns advent.day1
  (:require [clojure.java.io :as io])
  (:require [clojure.string :refer [split-lines]]))

; Project setup: Initially I used a leinigen based project, which turned out to be overkill, since I am not using
; external dependencies nor do I have a need to package this code.

; Conventions: Started off using camelCase for names, but the convention is snake-case,
; for instance see https://github.com/bbatsov/clojure-style-guide

; https://adventofcode.com/2018/day/1
(println "Working dir:" (System/getProperty "user.dir"))

(def input-vector
  ; Composing this as a pipeline with the "thread-first" -> macro cleans things up quite a bit.
  ; Initially I used normal evaluation and intermediate variables for this, which is less readable.
  ; -> inserts the first element in-between the function in the second element and any other arguments.
  ; day 2 has an example of the "thread-last" macro ->> which inserts it after other arguments.
  (-> "inputs/day1.txt"
       (io/file) ; You can read classpath resources with io/resource.
       (slurp)
       split-lines))

; Check that we can add two values
; Let expressions let us bind variables to a local scope
(let [elem-one (input-vector 0)
      elem-two (input-vector 1)
      sum (+ (read-string elem-one)
             (read-string elem-two))]
  (println
    "Input size: " (count input-vector) "\n"
    "Elem 1: " elem-one "\n"
    "Elem 2: " elem-two "\n"
    "Sum: " sum))

; map all to int then apply + so it sums them
(def final-sum
  (apply + (map read-string input-vector)))

(println "Final Sum: " final-sum)
; 592

; mapv does an eager evaluation, so vector-as-nums is a real vector and not a lazy seq
(def vector-as-nums (mapv read-string input-vector))
(def vector-length (count input-vector))

(defn search [i freq seen-frequencies]
  (cond
    (= i vector-length)
    ; recur acts like a recursive call ... presumably without adding to the call stack (?)
    ; The arity of the parameters has to match that of the enclosing function.
    (recur 0 freq seen-frequencies)

    (contains? seen-frequencies (+ freq (vector-as-nums i)))
    (+ freq (vector-as-nums i))

    ; conj does an efficient insert, which means it acts differently for lists vs vectors (i.e. insert at head vs at tail)
    :else
    (recur (inc i) (+ freq (vector-as-nums i)) (conj seen-frequencies (+ freq (vector-as-nums i))))))

; #{} is a set!
(println (search 0 0 #{}))
; 241