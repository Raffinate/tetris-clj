(ns tetris.engine
  (:require [tetris.game :as g]
            [clojure.core.async :as as])
  (:gen-class))
                                        ;(as/timeout )

;(defn iteration-delay [score]
;  (max 100 (quot 500 (inc (quot score 10)))))

(defn iteration-delay [score]
  (int (+ (/ 60000.0 (+ (* 5.0 score) 150.0)) 100)))

(defn run-engine [user-ch game-ch]
  (println "Starting engine...")
  (as/go-loop [game (g/make-game)
               iterate-time (+ (System/currentTimeMillis) (iteration-delay (:score game)))
               t-ch (as/timeout (iteration-delay (:score game)))]
    ;(println "asd")
    (let [[gm ch] (as/alt!
                    user-ch ([e]
                             (case e
                               :exit [:exit :user]
                               :new [:new :user]
                               [(g/apply-move game e) :user]))
                    t-ch ([_]
                          [(g/iterate-game game) :time]))]
      (cond
        (= gm :exit) nil
        (= gm :new) (recur (g/make-game)
                           (+ (System/currentTimeMillis)
                              (iteration-delay (:score game)))
                           (as/timeout (iteration-delay (:score game))))
        :else (do
          ;(g/print-game gm)
                (as/>! game-ch gm)
                (let [next-it-time (if (= ch :time)
                                     (+ iterate-time (iteration-delay (:score game)))
                                     iterate-time)]
                  (recur gm
                         next-it-time
                         (as/timeout (- next-it-time (System/currentTimeMillis))))))

        ))))
      ;; (if-not (= gm :exit)
        ;; (do
        ;;   ;(g/print-game gm)
        ;;   (as/>! game-ch gm)
        ;;   (let [next-it-time (if (= ch :time)
        ;;                        (+ iterate-time (iteration-delay (:score game)))
        ;;                        iterate-time)]
        ;;     (recur (ifgm
        ;;            next-it-time
        ;;            (as/timeout (- next-it-time (System/currentTimeMillis))))))
      ;;   nil))))

