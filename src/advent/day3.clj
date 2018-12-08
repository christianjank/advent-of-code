(ns advent.day3
  (:require [clojure.string :refer [split split-lines join]]
            [clojure.java.io :as io]))

(def input-vector
  (-> "inputs/day3.txt"
      (io/file)
      (slurp)
      (split-lines)))

; Initially used the following to do the extraction, but a single regex is better,
; plus retrieving data had to be done via the longer (get-in rect-data [:coords 0])
;(defn extract-point [pattern data]
;  (mapv #(Integer/parseInt %) (rest (first (re-seq pattern data)))))
;(defn extract-data [line]
;  (let [id-pattern #"(\d+)"
;        coords-pattern #"(\d+),(\d+):"
;        size-pattern #"(\d+)x(\d+)"]
;    (as-> line result
;          (split result #" ")
;          (zipmap [:id :atSign :coords :size] result)
;          {:id     (first (re-find id-pattern (:id result)))
;           :coords (extract-point coords-pattern (:coords result))
;           :size   (extract-point size-pattern (:size result))})))

; #123 @ 3,2: 5x4
; claimID 123, 3 inches from left, 2 from top, 5 width, 4 height
(defn parse-data-line [line]
  (let [regex #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"]
    (->> line
         (re-find regex)
         (rest)
         (map #(Integer/parseInt %))
         (zipmap [:id :xPos :yPos :width :height]))))       ; makes it easier to reference later (could have used a record?)

(defn write-to-file [string file]
  (spit (io/file file) string))

(defn matrix-to-string [matrix]
  (join "\n" (map #(join "\t" %) matrix)))

(def rectangle-data (map parse-data-line input-vector))
(def matrix-size 1000)
(def empty-matrix (vec (repeat matrix-size (vec (repeat matrix-size 0)))))

(println "Parsed data:" rectangle-data)
(write-to-file (matrix-to-string empty-matrix) "empty.txt")

(def clash! "X")
(defn place-rectangles [matrix rectangle-datum]
  (let [start-x (get rectangle-datum :xPos)
        start-y (get rectangle-datum :yPos)
        end-x (+ start-x (get rectangle-datum :width))
        end-y (+ start-y (get rectangle-datum :height))
        id (:id rectangle-datum)]
    (reduce
      (fn [field coords]
        (update-in
          field
          coords
          (fn [old] (if (= old 0) id clash!))))
      matrix
      (for
        [y (range start-y end-y)
         x (range start-x end-x)]
        [y x]))))

; Initially I tried something like the following, which was super slow (several minutes)
; since it traversed the entire matrix on every iteration.
;(map-indexed
;  (fn [y row]
;    (map-indexed
;      (fn [x elem]
;        (if
;          (and (<= start-x x) (< x end-x)
;               (<= start-y y) (< y end-y))
;          (if
;            (= elem 0)
;            rectId
;            "X")
;          elem))
;      row))
;  field)

(def filled-matrix (reduce place-rectangles empty-matrix rectangle-data))
(def field-to-string (matrix-to-string filled-matrix))
(def matrix-frequencies (frequencies (flatten filled-matrix)))

(write-to-file field-to-string "filled.txt")
(println "Frequencies:" matrix-frequencies)
(println "No of clashes:" (matrix-frequencies clash!))

; PART 2
(def id-to-size-map
  (reduce
    #(assoc %1 (%2 :id) (* (%2 :width) (%2 :height)))
    {}
    rectangle-data))

(def no-clash
  (filter #(= (val %) (id-to-size-map (key %)))
          matrix-frequencies))

(println "Rectangle without clash:" (key (first no-clash)))