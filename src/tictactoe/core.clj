(ns tictactoe.core
  (:gen-class))

;;; Utility

(defn in? [item coll]
  (some #(= item %) coll))

;;; I/O

(defn read-int []
  (try (Integer/parseInt (read-line))
    (catch NumberFormatException e nil)))

(defn user-prompt [x] (print x) (flush))

(defn- suffix [x]
  (cond (> x 7) nil
        (= 2 (rem x 3)) "\n-----------\n"
        :else "|"))

(defn- board-str [board] ; board must be a vector
  (apply str
    (map #(str " " (board %) " " (suffix %)) (range 9))))


;;; Players 

(def players {:x {:name "1" :mark "X"}
              :o {:name "2" :mark "O"}})

;;; Board Functions

(def win-sets
  [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]])

(defn empty-square? [square]
  (= " " square))

(defn make-board [] (vec (repeat 9 " ")))

(defn valid-move? [board move]
  (try (= (board move) " ")
    (catch Exception e false)))

(defn- board-win-sets [board]
  (map #(for [i %] (board i)) win-sets))

(defn winner-array? [sol]
  (and 
    (apply = sol)
    (not (= " " (first sol)))))

(defn winner-array [sol]
  (if (winner-array? sol)
    ({"X" :x "O" :o} (first sol))
    nil))

(defn winner [board] 
  (some #{:x :o} (map winner-array (board-win-sets board))))
  ;TODO assert one winning array

(defn final-message [board]
  (let [winning-player (winner board)]
    (if (or (= :x winning-player) (= :o winning-player))
      (str "Player " ((winning-player players) :name) " won!")
      "Cats game!")))

(defn full? [board]
  (not-any? empty-square? board))

(defn game-over? [board] 
  (or (winner board) (full? board)))

(defn place-on-board [board spot mark] 
  (assoc board spot mark))

(defn- get-next-move [board player]
  (println)
  (println (board-str board))
  (user-prompt (str "\nPlayer " (player :name) ", pick your space (1-9): "))
  (let [input (read-int)]
    (if (in? input (range 1 9))
      (dec input)
      (recur board player))))

(defn- move [board player]
  (let [move (get-next-move board player)]
    (if (valid-move? board move)
      (place-on-board board move (:mark player))
      (recur board player))))

(defn- instructions-str []
  (str "\n"
    "Welcome to Tic-Tac-Toe!\n\n"
    "To move enter one of the numbers within the grid\n\n"
    (board-str (vec (range 1 10)))
    "\n"))

;;; Driver

(defn console-play []
  (println (instructions-str))
  (loop [board (make-board) current-player (:x players) next-player (:o players)]
    (if (game-over? board)
      (do
        (println (str "\n" (final-message board) "\n"))
        (println (board-str board)))
      (recur
        (move board current-player) next-player current-player))))

(defn -main [& args]
  (console-play))
