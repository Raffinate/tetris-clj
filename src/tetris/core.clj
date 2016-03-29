(ns tetris.core
  (:import (java.awt Color))
  (:require [tetris.swing :as gui]
            [tetris.engine :as e]
            [tetris.game :as g]
            [clojure.core.async :as as]
            )
  (:gen-class))

(defn fig->color [fig]
  (get {:i Color/CYAN
        :j Color/BLUE
        :l Color/ORANGE
        :o Color/YELLOW
        :s Color/GREEN
        :t Color/MAGENTA
        :z Color/RED} fig (new Color 200 200 200)))

(defn cells->colors [cells]
  (into {} (for [[k v] cells] [k (fig->color v)])))

(defn dropped-figure->cells [dropped-figure type]
  (zipmap dropped-figure (repeat type)))

(defn game->render-state [game]
  ;(g/print-game game)
  (let [dg (if (= (:state game) :game) (g/fall-block game) game)]
    {:board-dim [(get-in game [:board :x])
                 (get-in game [:board :y])]
     :max-fig-coord 4
     :score (:score game)
     :cells (cells->colors (g/game-cells game))
     :drop-cells (cells->colors (dropped-figure->cells
                                 (get-in dg [:board :figure])
                                 (get-in dg [:board :figure-key])))
     :next-cells (cells->colors (g/next-figure-cells game))}))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (let [user-ch (as/chan)
        game-ch (as/chan 1 (map game->render-state))]
    (gui/run-gui user-ch game-ch)
    (future (e/run-engine user-ch game-ch))))


