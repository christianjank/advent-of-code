(ns advent.day2
  (:require [clojure.java.io :as io])
  (:require [clojure.string :refer [split-lines]]))

; https://adventofcode.com/2018/day/2
(def input-vector
  (-> "inputs/day2.txt"
      (io/file)
      (slurp)
      (split-lines)))

; You can destructure parameters like this. In this case a 2 elem vector (conventional way of doing tuples)
; where we ignore the first element with the _.
(defn has-freq-2? [[_ num]] (= num 2))
(defn has-freq-3? [[_ num]] (= num 3))
(defn create-two-three-tuple [letter-frequencies]
  ; Returned value is a two element vector of booleans, e.g. [true false].
  [(some has-freq-2? letter-frequencies)
   (some has-freq-3? letter-frequencies)])

; using the "thread-last" macro (explained in a day 1 comment)
; here we see why we can't use the ->, because it would insert the input-vector in-between "map" and "frequencies"
(def two-three-tuple
  (->> input-vector
       (map frequencies)
       (map create-two-three-tuple)))

(defn true-to-1 [b] (if b 1 0))
(defn add-to-sum [current-sum truth-value] (+ current-sum (true-to-1 truth-value)))
(defn sum-truth-values [[sumOf2s sumOf3s] [has-freq-2 has-freq-3]] ; more destructuring
  [(add-to-sum sumOf2s has-freq-2)
   (add-to-sum sumOf3s has-freq-3)])

(def sums
  (reduce sum-truth-values
          [0 0]
          two-three-tuple))
(def checksum (apply * sums))
; strangely, println inserts a space after each argument
(println "Sum of 2s:" (sums 0) "\nSum of 3s:" (sums 1))
(println "Checksum:" checksum)
; 5704

(defn find-no-of-differences [left right difference-count]
  (cond
    ; gotta use empty? not nil?
    (empty? left)
    difference-count

    (= (first left) (first right))
    (recur (rest left) (rest right) difference-count)

    :else
    (recur (rest left) (rest right) (inc difference-count))))

(defn find-close-matches [lines]
  (if
    (empty? (rest lines))
    nil

    (let [line-to-compare (first lines)
          other-lines (rest lines)
          has-one-char-difference? (fn [rightLine] (= 1 (find-no-of-differences line-to-compare rightLine 0)))
          possible-matches (filter has-one-char-difference? other-lines)]
      (cond
        (= 1 (count possible-matches))
        [line-to-compare (first possible-matches)]

        :else
        (recur other-lines)))))

(defn strip-mismatch [left right chars-acc]
  (cond
    (empty? left)
    (clojure.string/join chars-acc)

    (= (first left) (first right))
    (recur (rest left) (rest right) (conj chars-acc (first left)))

    :else
    (recur (rest left) (rest right) chars-acc)))

(def matches (find-close-matches input-vector))
(println "Close matches:" matches)

; funny how the matches vector is also a function that can be applied to these index values
(def stripped-string (strip-mismatch (matches 0) (matches 1) []))
(println "Match stripped of differences:" stripped-string)