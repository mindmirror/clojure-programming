(ns game-of-life)
(use 'clojure.pprint)

(defn empty-board
  "Creates a rectangular empty board of the specified width and height."
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  "Turns :on each of the cells specified as [y, x] coordinates."
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))

(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))

(pprint glider)

(defn neighbors
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn count-neighbors
  [board loc]
  (count (filter #(get-in board %) (neighbors loc))))

(defn indexed-step
  "Yields the next state of the board, using indices to determine neighbors, liveness, etc."
  [board]
  (let [w (count board)
        h (count (first board))]
    (loop [new-board board x 0 y 0]
      (cond
       (>= x w) new-board
       (>= y h) (recur new-board (inc x) 0)
       :else
       (let [new-liveness
             (case (count-neighbors board [x y])
               2 (get-in board [x y])
               3 :on
               nil)]
         (recur (assoc-in new-board [x y] new-liveness) x (inc y)))))))

(-> (iterate indexed-step glider) (nth 8) pprint)

(defn indexed-step2
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
     (fn [new-board x]
       (reduce
        (fn [new-board y]
          (let [new-liveness
                (case (count-neighbors board [x y])
                  2 (get-in board [x y])
                  3 :on
                  nil)]
            (assoc-in new-board [x y] new-liveness)))
        new-board (range h)))
     board (range w))))

(-> (iterate indexed-step2 glider) (nth 8) (pprint))

(defn indexed-step3
  [board]
  (reduce (fn [new-board [x y]]
            (let [new-liveness
                  (case (count-neighbors new-board [x y])
                    2 (get-in board [x y])
                    3 :on
                    nil)]
              (assoc-in new-board [x y] new-liveness)))
          board
          (for [x (range (count board)) y (range (count (first board)))] [x y])))

(-> (iterate indexed-step3 glider) (nth 8) (pprint))


;;---------------------------------------------------------------------
;; Loop-less version
;;---------------------------------------------------------------------

(partition 3 1 (range 5))
(partition 3 1 (concat [nil] (range 5) [nil]))

(defn window
  "Returns a lazy sequence of 3-item windows centered around each item of coll."
  [coll]
  (partition 3 1 (concat [nil] coll [nil])))

(defn cell-block
  "Creates a sequence of 3x3 windows of a triple of 3 sequences."
  [[left mid right]]
  (window (map vector
               (or left (repeat nil)) mid (or right (repeat nil)))))

(defn window2
  "Returns a lazy sequence of 3-item windows centered around each item of coll, padded as necessary with pad or nil."
  ([coll] (window2 nil coll))
  ([pad coll]
   (partition 3 1 (concat [pad] coll [pad]))))

(defn cell-block2
  "Cerates a sequence of 3x3 windows from a triple of 3 sequences."
  [[left mid right]]
  (window2 (map vector left mid right)))

(defn liveness
  "Returns the lvieness (nil or :on) of the center cell for the next step."
  [block]
  (let [[_ [_ center _] _] block]
    (case (- (count (filter #{:on} (apply concat block)))
             (if (= :on center) 1 0))
      2 center
      3 :on
      nil)))

(defn- step-row
  "Yields the next state of the center row."
  [rows-triple]
  (vec (map liveness (cell-block2 rows-triple))))

(defn index-free-step
  "Yies the next state of the board."
  [board]
  (vec (map step-row (window2 (repeat nil) board))))

(index-free-step glider)

(pprint glider)
(def s1 (window2 (take 11 (repeat nil)) glider))
(pprint s1)
(def s2 (first s1))
(pprint s2)
(pprint (cell-block2 s2))
(pprint (map cell-block2 s1))

(= (nth (iterate indexed-step glider) 8)
   (nth (iterate index-free-step glider) 8))


