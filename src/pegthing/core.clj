(ns pegthing.core
  (:gen-class))

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

(def complementary-dirs
  {3 0
   4 1
   5 2})

(defn new-board
  []
  {})

(defn adjacent-positions
  [pos]
  (if (triangular? pos)
    [2 3]
    [1 2 3]))

(defn add-peg
  [board row-num]
  )

(defn add-row
  [board row-num]
  )

(defn bigraph-assoc
  [board p1 p2 dir])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
