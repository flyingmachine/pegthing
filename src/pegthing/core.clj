(ns pegthing.core
  (require [clojure.set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
     (let [new-sum (+ sum n)]
       (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  [n]
  (= n (last (take-while #(>= n %) tri))))

(def rows
  (cons '(1)
        (map (partial apply concat)
             (partition-all 2 (rest (partition-by triangular? (iterate inc 1)))))))

(def axes [:a :b :c])

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

(defn add-pos
  [board row-num pos]
  (let [board (assoc-in board [pos :pegged] true)]
    (reduce (fn [board [axis neighbor-pos]]
              (join-positions board pos neighbor-pos axis))
            board
            (neighbors row-num pos))))

(defn row-positions
  [row-num]
  (nth rows (dec row-num)))

(defn add-row
  [board row-num]
  (reduce (fn [board pos] (add-pos board row-num pos))
          board
          (row-positions row-num)))

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

(defn remove-last-row
  "board creation process results in an extra row; remove it"
  [board]
  (reduce (fn [board pos] (dissoc board pos))
          board
          (row-positions (inc (:rows board)))))

(defn new-board
  [rows]
  (remove-last-row (add-rows {:rows rows})))

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
  (clojure.string/join " " (map (partial render-pos board) (row-positions row-num))))

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (let [row-string (render-row board row-num)
          padding (row-padding row-string (:rows board))]
      (println padding row-string))))

(defn jump-pos
  "Next position when p1 tries to jump p2"
  [board p1 p2 axis]
  (first (disj (get-in board [p2 axis]) p1)))

(defn can-jump?
  "Can pos jump over connected-pos? If so, returns position it would
  jump to"
  [board pos axis connected-pos]
  (let [jump-to (jump-pos board pos connected-pos axis)]
    (and (get-in board [connected-pos :pegged])
         (get board jump-to)
         (not (get-in board [jump-to :pegged]))
         jump-to)))

(defn valid-moves
  [board pos]
  (let [meta (get board pos)]
    (set (mapcat (fn [axis]
                   (filter identity
                           (map (partial can-jump? board pos axis)
                                (axis meta))))
                 axes))))

(defn between
  [board axis p1 p2]
  (first (clojure.set/intersection (get-in board [p1 axis])
                                   (get-in board [p2 axis]))))

(defn valid-move?
  [board p1 p2]
  (and (contains? (valid-moves board p1) p2)
       (some #(between board % p1 p2) axes)))

(defn remove-peg
  [board p]
  (assoc-in board [p :pegged] false))

(defn add-peg
  [board p]
  (assoc-in board [p :pegged] true))

(defn move-peg
  [board p1 p2]
  (add-peg (remove-peg board p1) p2))

(defn get-input
  ([] (get-input nil))
  ([default]
     (let [input (clojure.string/trim (read-line))]
       (if (empty? input)
         default
         (clojure.string/lower-case input)))))

(defn characters-as-strings
  [string]
  (re-seq #"[a-zA-Z]" string))

(defn make-move
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn prompt-move
  [board]
  (println "\n\nYour board:\n")
  (print-board board)
  (println "\nMove from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (successful-move new-board)
      (do
        (println "That was an invalid move :(")
        (prompt-move board)))))

(defn can-move?
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

(defn successful-move
  [board]
  (if (can-move? board)
    (do
      (print-board board)
      (prompt-move board))
    (game-over board)))

(defn game-over
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left.")
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (query-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

(defn query-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn query-rows
  []
  (println "How many rows? [5]")
  (let [rows (get-input 5)
        board (new-board rows)]
    (query-empty-peg board)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Get ready to play peg thing!")
  (query-rows))
