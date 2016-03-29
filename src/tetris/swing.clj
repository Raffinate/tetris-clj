(ns tetris.swing
  (:import
   (java.awt Container Dimension Color Graphics Graphics2D Rectangle)
   (java.awt.geom AffineTransform)
   (java.awt.image BufferedImage)
   (java.awt.event KeyListener KeyEvent)
   (javax.swing JFrame JPanel))
  (:require [clojure.core.async :as as])
  (:gen-class))

(def debug-time (atom (System/currentTimeMillis)))

(defn dark-color [clr]
  (let [drk (fn [i]
              (int (quot i 1.2)))]
    (new Color
          (drk (.getRed clr))
          (drk (.getGreen clr))
          (drk (.getBlue clr))
          (.getAlpha clr))))

(defn transparent-color [clr]
  (new Color
         (.getRed clr)
         (.getGreen clr)
         (.getBlue clr)
         50))

(defn new-color [r g b]
  (new Color r g b))

(defn get-action [k]
  (get {KeyEvent/VK_SPACE :drop
        KeyEvent/VK_LEFT :left
        KeyEvent/VK_UP :rotate
        KeyEvent/VK_RIGHT :right
        KeyEvent/VK_DOWN :iterate
        KeyEvent/VK_ESCAPE :new} k))

(defn new-key-listener [user-ch]
  (let [kl (proxy [KeyListener] []
             (keyPressed [e]
               (let [kc (.getKeyCode e)
                     action (get-action kc)]
                 ;(println "Pressed: " kc)
                 (when action
                   ;(reset! debug-time (System/currentTimeMillis))
                   (do (as/go (as/>! user-ch action))))))
             (keyTyped [e] nil)
             (keyReleased [e] nil))]
    kl))
(defn dark-color [clr]
  (let [drk (fn [i]
              (int (quot i 1.2)))]
    (new Color
          (drk (.getRed clr))
          (drk (.getGreen clr))
          (drk (.getBlue clr))
          (.getAlpha clr))))

(defn transparent-color [clr]
  (new Color
         (.getRed clr)
         (.getGreen clr)
         (.getBlue clr)
         50))


(defn model->real [size p]
  (mapv (partial * size) p))

(defn shift-point [s1 s2]
  (mapv + s1 s2))

(defn center-render-area [actual-dim render-dim]
  (map #(quot (- %1 %2) 2) actual-dim render-dim))

(defn calculate-cell-size [render-dim model-dim]
  (try
    (apply min (map quot render-dim model-dim))
    (catch Exception e (println e render-dim model-dim)
           (throw e))))

(defn new-rectangle
  ([[x y :as real-p] [w h :as real-dim] clr]
   {:shape (new Rectangle x y w h) :color clr}))

(defn draw-gobject [g {:keys [shape color]}]
  (.setColor g color)
  (.fill g shape))

(defn translate-gobject [[x y :as p] gobj]
  (update gobj :shape (fn [s] (.translate s x y) s)))

(defn translate-gobjects [p objs]
  (doall (mapv (partial translate-gobject p) objs)))

(defn render-cell [p size color]
  (let [border (quot size 10)
        cell-color (dark-color color)
        border-color color
        p (model->real size p)
        d [size size]
        ]
    [(new-rectangle p d border-color)
     (new-rectangle (shift-point p [border border])
                    (shift-point d (mapv (partial * -2) [border border]))
                    cell-color)]))

(defn render-background [real-dim]
  [(new-rectangle [0 0] real-dim Color/BLACK)])

(defn render-cells [cells size color-transform]
  (reduce into [] (mapv (fn [[p c]]
                          (render-cell p size (color-transform c))) cells)))

(defn render-rectangle [model-pos model-dim size color]
  (let [to-real (partial model->real size)]
    [(new-rectangle (to-real model-pos) (to-real model-dim) color)]))

(defn render-board [[dimx dimy :as model-dim] y-pad cell-size cells drop-cells]
  (let [to-real (partial model->real cell-size)]
    (reduce into
            [(new-rectangle (to-real [0 (- y-pad)])
                            (shift-point (to-real [dimx y-pad]) [0 -1]) Color/WHITE)
             (new-rectangle [0 0] (to-real model-dim) Color/WHITE)]
            [(render-cells cells cell-size identity)
             (render-cells drop-cells cell-size transparent-color)])))

(defn render-next-figure [background-size cells cell-size]
  (into (render-rectangle [0 (- background-size)]
                          [background-size background-size]
                          cell-size
                          Color/WHITE)
        (translate-gobjects [0 (- cell-size)]
                            (render-cells cells cell-size identity))))

(defn render-state [{:keys [graphics canvas-dim
                            board-dim
                            max-fig-coord score
                            cells
                            next-cells
                            drop-cells
                            score] :as state}]
  (let [full-model-dim (reduce shift-point board-dim [[max-fig-coord max-fig-coord] [3 2]])
        fig-cell-size (calculate-cell-size canvas-dim full-model-dim)
        to-real (partial model->real fig-cell-size)
        model-dim (shift-point full-model-dim [-2 -2])
        basic-shift (center-render-area canvas-dim
                                        (to-real model-dim))
        board-shift (shift-point basic-shift (to-real [0 max-fig-coord]))
        next-fig-shift (shift-point board-shift (to-real [(inc (first board-dim)) 0]))
        ]
    (doall (map (partial draw-gobject graphics)
                (reduce into (render-background canvas-dim)
                        [(translate-gobjects board-shift
                                             (render-board board-dim
                                                           max-fig-coord
                                                           fig-cell-size
                                                           cells drop-cells))
                         (translate-gobjects next-fig-shift
                                             (render-next-figure max-fig-coord
                                                                 next-cells fig-cell-size))
                         ])))
    (.setColor graphics Color/CYAN)
    (.drawString graphics (str "Score: \n" score) (first basic-shift)
                 (second basic-shift))))

(defn new-board [user-ch g-state]
  (let [panel (proxy [JPanel] []
                (paintComponent [g]
                  ;;(paint-board this g @cells)
                  (let [state @g-state]
                    (try
                      (when-not (nil? state)
                        (render-state (assoc state
                                             :graphics g
                                             :canvas-dim [(.getWidth this)
                                                          (.getHeight this)]
                                             )))
                      (catch Exception e (println e) (println state))))
                  ;(println "Response time: " (- (System/currentTimeMillis) @debug-time))
                  ))]
    (doto panel
      (.setFocusable true)
      (.requestFocusInWindow true)
      (.addKeyListener (new-key-listener user-ch)))))


(defn new-frame [user-ch g-state]
  (let [frame (proxy [JFrame] []
                (dispose []
                  (as/go (as/>! user-ch :exit))
                  (println "Goodbye...")
                  (proxy-super dispose)))
        cp (.getContentPane frame)]
    (doto cp
      (.setPreferredSize (new Dimension 400 600))
      (.add (new-board user-ch g-state)))
    (doto frame
      (.setTitle "Tetris")
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.pack)
      (.show))))

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

(defn run-gui [user-ch cell-ch]
  (let [g-state (atom nil)
        fr (new-frame user-ch g-state)]
    (as/go-loop []
      (let [state (as/<! cell-ch)]
        (reset! g-state state)
        (.repaint fr))
      (recur))))
