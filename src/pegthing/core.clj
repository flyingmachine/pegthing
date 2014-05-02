(ns pegthing.core
  (:gen-class))

;; TODO explain why it's nice to start at 0
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

(def complementary-dirs
  {2 5
   3 6
   4 1})

(defn join-right
  [row-num board pos]
  (assoc-in board [pos 2] (inc pos)))

(defn join-down-right
  [row-num board pos]
  (assoc-in board [pos 3] (+ pos row-num 1)))

(defn join-down-left
  [row-num board pos]
  (assoc-in board [pos 4] (+ pos row-num)))

(defn adjacent-joiners
  [board row-num]
  [(partial join-right row-num)
   (partial join-down-left row-num)
   (partial join-down-right row-num)])

(defn add-peg
  [board row-num pos joiners]
  (let [joiners (if (triangular? pos) (rest joiners) joiners)
        board (assoc-in board [pos :pegged] false)]
    (reduce (fn [board joiner] (joiner board pos))
            board
            joiners)))

(defn peg-positions
  [row-num]
  (map inc (range (nth tri (dec row-num))
                  (nth tri row-num))))

(defn add-row
  [board row-num]
  (reduce add-peg board (peg-positions row-num) (adjacent-joiners board row-num)))

(defn add-rows
  [board rows]
  (reduce add-row board (range 1 rows)))

(defn add-last-row
  [board rows]
  )

(defn new-board
  [rows]
  (let [board {}]
    (merge (add-rows board rows)
           (add-last-row board rows))))

(defn bigraph-assoc
  [board p1 p2 dir])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
