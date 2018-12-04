(ns advent.day2
  (:require [clojure.java.io :as io])
  (:require [clojure.string :refer [split-lines]]))

; https://adventofcode.com/2018/day/2
(def input-vector
  (-> "inputs/day2.txt"
       (io/file)
       (slurp)
       (split-lines)))

(defn has-freq-2? [[_ num]] (= num 2))
(defn has-freq-3? [[_ num]] (= num 3))
(defn create-two-three-tuple [letter-frequencies]
  [(some has-freq-2? letter-frequencies)
   (some has-freq-3? letter-frequencies)])

(def two-three-tuple
  (->> input-vector
       (map frequencies)
       (map create-two-three-tuple)))

(defn true-to-1 [b] (if b 1 0))
(defn add-to-sum [current-sum truth-value] (+ current-sum (true-to-1 truth-value)))
(defn sum-truth-values [[sumOf2s sumOf3s] [has-freq-2 has-freq-3]]
  [(add-to-sum sumOf2s has-freq-2)
   (add-to-sum sumOf3s has-freq-3)])

(def sums
  (reduce sum-truth-values
          [0 0]
          two-three-tuple))
(def checksum (apply * sums))
(println "Sum of 2s:" (sums 0) "\nSum of 3s:" (sums 1))
(println "Checksum:" checksum)
; 5704

(defn find-no-of-differences [left right difference-count]
  (cond
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
          possible-matches (filter
                            (fn [rightLine] (= 1 (find-no-of-differences line-to-compare rightLine 0)))
                            other-lines)]
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

(def stripped-string (strip-mismatch (matches 0) (matches 1) []))
(println "Match stripped of differences:" stripped-string)