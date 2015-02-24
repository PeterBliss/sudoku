(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [row-idx (first coord)
        columns (range 9)
        row-coords (map #(vector row-idx %) columns)]
    (set (distinct (map #(value-at board %) row-coords)))))

(defn col-values [board coord]
  (let [col-idx (second coord)
        rows (range 9)
        col-coords (map #(vector % col-idx) rows)]
    (set (distinct (map #(value-at board %) col-coords)))))

(defn coord-pairs [coords]
  (for [row coords 
        col coords]
    [row col]))

(defn top-left-corner [coord]
  (let [row (first coord) row-block-num (int (/ row 3)) 
        row-corner (* 3 row-block-num)
        col (second coord) col-block-num (int (/ col 3)) 
        col-corner (* 3 col-block-num)]
    [row-corner col-corner]))

(defn run-test [f input exp-result]
  (let [result (f input)]
    (if (= result exp-result)
      (println "test passed")
      (println "test FAILED - expected" exp-result "got" result))))

(defn block-values [board coord]
  (let [tlc (top-left-corner coord) ; top left corner of block coord
        offsets (coord-pairs [0 1 2]) ; offsets from tlc
        map-fn (fn [elem] 
                 [(+ (first elem) (first tlc)) (+ (second elem) (second tlc))])
        block-coords (map map-fn offsets)] ; tlc plus offsets
    (set (distinct (map #(value-at board %1) block-coords)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    '#{}
    (let [all-cohort-vals (clojure.set/union (row-values board coord)
                                             (col-values board coord)
                                             (block-values board coord))]
    (clojure.set/difference all-values all-cohort-vals))))

(defn filled? [board]
  (let [row-empty? 
        (fn [row] 
          (not (contains? (set (map zero? (first board))) true)))
        t-f-vals (set (map row-empty? board))]
    (contains? t-f-vals true)))

(defn rows [board]
  (map #(row-values board (vector %1 0)) (range 9)))

(defn valid-rows? [board]
  (let [rows (rows board)
        valid-row? (fn [r] 
                    (= 0 (count (clojure.set/difference all-values r))))]
    (= 9 (count (filter valid-row? rows)))))

(defn cols [board]
  (map #(col-values board (vector 0 %1)) (range 9)))

(defn valid-cols? [board]
  (let [cols (cols board)
        valid-col? (fn [c] 
                    (= 0 (count (clojure.set/difference all-values c))))]
    (= 9 (count (filter valid-col? cols)))))

(defn blocks [board]
  (let [coords (for [row [0 3 6] col [0 3 6]]
                 (vector row col))]
        (map #(block-values board %) coords)))

(defn valid-blocks? [board]
  (let [blocks (blocks board)
        valid-block? (fn [b] 
                    (= 0 (count (clojure.set/difference all-values b))))]
    (= 9 (count (filter valid-block? blocks)))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (coord-pairs (range 9))]
    (first (filter #(not (has-value? board %)) coords))))

(defn solve-helper [board]
  (if (valid-solution? board)
    board
    (let [empty-pt (find-empty-point board)
          poss-vals (valid-values-for board empty-pt)
          ];j (println empty-pt "can be" poss-vals)]
      (for [val poss-vals
            soln (solve-helper (assoc-in board empty-pt val))]
        soln))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (solve-helper board)))

(def almost-solved-board-1
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [0 4 5 2 8 6 1 7 9]]))

(def almost-solved-board-2
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 0 0 0 1 4 2 3]
          [4 2 0 8 5 3 7 9 1]
          [7 1 0 0 2 4 8 5 6]
          [0 0 0 5 3 7 2 8 4]
          [0 0 0 4 1 9 6 3 5]
          [0 0 0 0 0 0 1 7 9]]))

