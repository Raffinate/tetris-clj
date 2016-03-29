(ns tetris.game
  (:require [tetris.board :as b])
  (:gen-class))

(defn make-game
  ([]
   (make-game (b/make-board) b/*figures* (repeatedly #(rand-nth (keys b/*figures*)))))
  ([board figures fig-order]
   {:board board
    :figures figures
    :score 0
    :fig-order fig-order
    :state :start ;:game :remove :end
    }))


(defmulti iterate-game (fn [game] (:state game)))

(defmethod iterate-game :start [game]
  (let [board (:board game)
        fig-type (first (:fig-order game))
        rotations (get (:figures game) fig-type)
        fig (b/new-figure rotations (get board :init-shift))]
    (if (b/check-figure (b/board-points board) (:y board) fig)
      (assoc game :state :end)
      (iterate-game (-> game
                        (assoc :state :game)
                        (update :board b/set-figure rotations fig fig-type)
                        (update :fig-order next))))))

(defmethod iterate-game :game [game]
  (let [new-board (b/board-down (:board game))
        new-game (assoc game :board new-board)]
    (if (nil? (:figure-key new-board))
      (if (not (empty? (b/full-lines (:lines-fill new-board) (:x new-board))))
        (assoc new-game :state :remove)
        (if (b/board-full? new-board)
          (assoc new-game :state :end)
          (assoc new-game :state :start)))
      new-game)))

(defmethod iterate-game :remove [{:keys [board] :as game}]
  (let [removed (count (b/full-lines (:lines-fill board) (:x board)))
        new-board (b/board-clear-full-lines board)
        new-game (-> game
                     (assoc :board new-board)
                     (update :score + removed))]
    (if (b/board-full? new-board)
      (assoc new-game :state :end)
      (assoc new-game :state :start))))

(defmethod iterate-game :default [game]
  (assoc game :state :end))

(defn fall-block [game]
  (loop [game game]
    (let [g (iterate-game game)]
      (if (= (:state g) :game)
        (recur g)
        game))))

(defn drop-block [game]
  (if (= (:state game) :game)
    (iterate-game (fall-block game))
    game))

(defn apply-move [game mv]
  ;(println "Move: " mv)
  (cond
    (= mv :drop) (drop-block game) ;(drop-block game)
    (= mv :iterate) (iterate-game game)
    (= (:state game) :game) (update game :board b/apply-move mv)
    true game))

(defn apply-moves [game & mvs]
  (reduce apply-move game mvs))

(defn game-cells [game]
  (b/board-cells (:board game)))

(defn next-figure-cells [game]
  (let [nf (first (:fig-order game))
        points (first (get b/*figures* nf []))]
    (zipmap points (repeat nf))))

(defn print-game [game]
  (b/print-board (:board game))
  (println "Next: " (first (:fig-order game)))
  (println "Score: " (:score game))
  (println "State: " (:state game)))

