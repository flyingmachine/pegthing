(ns pegthing.core
  (require [clojure.set])
  (:gen-class))

;; TODO explain why it's nice to start at 0
;; used to produce range of peg positions on first row
(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 0))
  ([sum n]
     (let [new-sum (+ sum n)]
       (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn add-connection
  [new-connection connections]
  (set (conj connections new-connection)))

(defn join-positions
  [board p1 p2 axis]
  (reduce (fn [board [p1 p2]]
            (update-in board [p1 axis] (partial add-connection p2)))
          board
          [[p1 p2]
           [p2 p1]]))

(defn neighbors
  [row-num pos]
  (conj (if-not (triangular? pos) [[:a (inc pos)]])
        [:b (+ row-num pos)]
        [:c (+ row-num pos 1)]))

(defn add-peg
  [board row-num pos]
  (let [board (assoc-in board [pos :pegged] true)]
    (reduce (fn [board [axis neighbor-pos]]
              (join-positions board pos neighbor-pos axis))
            board
            (neighbors row-num pos))))

(defn peg-positions
  [row-num]
  (map inc (range (nth tri (dec row-num))
                  (nth tri row-num))))

(defn add-row
  [board row-num]
  (reduce (fn [board pos] (add-peg board row-num pos))
          board
          (peg-positions row-num)))

(defn add-rows
  [board]
  (reduce add-row board (range 1 (inc (:rows board)))))

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)
(defn letter->pos
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn new-board
  [rows empty-pos-letter]
  (let [board {:rows rows}]
    (assoc-in (add-rows board)
              [(letter->pos empty-pos-letter) :pegged]
              false)))

;; printing the board
(defn row-padding
  [row-string rows]
  (let [max-row-chars (* rows pos-chars)
        pad-length (/ (- max-row-chars (count row-string)) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged]) "0" "-")))

(defn render-row
  [board row-num]
  (clojure.string/join " " (map (partial render-pos board) (peg-positions row-num))))

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (let [row-string (render-row board row-num)
          padding (row-padding row-string (:rows board))]
      (println padding row-string))))

(defn between
  [board axis p1 p2]
  (first (clojure.set/intersection (get-in board [p1 axis])
                                   (get-in board [p2 axis]))))

(defn valid-move?
  [board l1 l2]
  (let [p1 (letter->pos l1)
        p2 (letter->pos l2)]
    (and
     (get-in board [p1 :pegged])
     (not (get-in board [p2 :pegged]))
     (some (fn [axis]
             (if-let [jumped (between board axis p1 p2)]
               (and (get-in board [jumped :pegged])
                    jumped)))
           [:a :b :c]))))

(defn make-move
  [board l1 l2]
  (if-let [jumped (valid-move? board l1 l2)]
    
    ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
