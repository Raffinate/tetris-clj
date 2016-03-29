(ns tetris.board
  (:gen-class))

(def ^:dynamic *figures*
  {:i (cycle [[[1 0] [1 -1] [1 -2] [1 -3]]
              [[0 0] [1 0] [2 0] [3 0]]])
   :t (cycle [[[0 -1] [1 -1] [2 -1] [1 0]]
              [[1 -2] [1 0] [1 -1] [2 -1]]
              [[0 -1] [1 -1] [2 -1] [1 -2]]
              [[0 -1] [1 -2] [1 -1] [1 0]]])
   :o (cycle [[[1 0] [2 0] [1 -1] [2 -1]]])
   :j (cycle [[[0 -1] [1 -1] [2 -1] [2 0]]
              [[1 0] [1 -1] [1 -2] [2 -2]]
              [[0 -1] [1 -1] [2 -1] [0 -2]]
              [[0 0] [1 0] [1 -1] [1 -2]]])
   :l (cycle [[[0 0] [0 -1] [1 -1] [2 -1]]
              [[1 0] [2 0] [1 -1] [1 -2]]
              [[0 -1] [1 -1] [2 -1] [2 -2]]
              [[1 0] [1 -1] [1 -2] [0 -2]]])
   :s (cycle [[[0 0] [1 0] [1 -1] [2 -1]]
              [[1 -1] [1 -2] [2 0] [2 -1]]])
   :z (cycle [[[0 -1] [1 -1] [1 0] [2 0]]
              [[0 0] [0 -1] [1 -1] [1 -2]]])})


(defn make-board []
  {:x 10
   :y 20
   :init-shift [3 -1]
   :cells {} ;map of [x y] :fig-type
   :figure-key nil ;:fig-type
   :figure-rotations nil ;:infinite lazy rotation sequence
   :figure [] ;:vector of points
   :lines-fill {} ;map of line and filled cell amount
   })

(defn map-into [f c & colls]
  (into (empty c) (apply map f c colls)))

(defn remove-into [f c]
  (into (empty c) (remove f c)))

(defn find-coord
  "Use it to find min and max :x and :y coordinates of cells."
  [figure coord selector]
  (let [c (if (= coord :x) first second)]
    (reduce selector (map c figure))))


(defn shift-point [shift p]
  (map-into + shift p))

(defn shift-figure [shift figure]
  (map-into (partial shift-point shift) figure))

(defn fit-figure [dimx figure]
  (let [findc (partial find-coord figure :x)
        figmin (findc min)
        figmax (findc max)
        dleft (when (< figmin 0) (- figmin))
        dright (when (>= figmax dimx) (- dimx figmax 1))]
    (let [shift (some identity [dleft dright])]
      (if shift
        (shift-figure [shift 0] figure)
        figure))))

(defn move-figure [figure direction]
  (let [shift (case direction
                :left [-1 0]
                :right [1 0]
                :down [0 1]
                [0 0])]
    (shift-figure shift figure)))

(defn rotate-figure [rotations figure]
  (let [shift (map-into -
                        (first figure)
                        (first (first rotations)))]
    (shift-figure shift (second rotations))))

(defn check-collision [points figure]
  (some points figure))

(defn board-points [board]
  (into #{} (keys (:cells board))))

(defn check-bottom [dimy figure]
  (>= (find-coord figure :y max) dimy))

(defn check-figure [points dimy figure]
  (or (check-collision points figure) (check-bottom dimy figure)))

(defn figure-lines-fill [figure lines-fill]
  (reduce (fn [lf p]
            (update lf (second p)
                    #(if (nil? %) 1 (inc %))))
          lines-fill
          figure))

(defn add-cells [board figtype figure]
  (-> board
      (update :cells conj (zipmap figure (repeat figtype)))
      (update :lines-fill (partial figure-lines-fill figure))))

(defn clear-figure [board]
  (-> board
      (assoc :figure-key nil)
      (assoc :figure-rotations nil)
      (assoc :figure [])))

(defn remove-line [board line-num]
  (-> board
      (update :lines-fill dissoc line-num)
      (update :lines-fill (partial map-into
                                   #(if (> (first %) line-num)
                                      %
                                      (shift-point [1 0] %))))
      (update :cells (partial remove-into #(= (second (first %)) line-num)))
      (update :cells (partial map-into
                              #(if (> (second (first %)) line-num)
                                 %
                                 [(shift-point [0 1] (first %)) (second %)])))))

(defn print-board [board]
  (let [cells (:cells board)]
    (doall (for [y (range (:y board)) x (range (:x board))]
      (let [p [x y]]
        (cond
          (contains? cells p) (print (name (get cells p)))
          (some #(= p %) (:figure board))  (print "*");(name (:figure-key board)))
          true (print "_"))
        (when (= x (dec (:x board)))
          (println ""))))))
  nil)

(defn new-figure [rotations init-shift]
  (shift-figure init-shift (first rotations)))

(defn set-figure [board rotations figure figtype]
  (-> board
      (assoc :figure figure)
      (assoc :figure-rotations rotations)
      (assoc :figure-key figtype)))

;; (defn add-figure-raw [board rotations figtype init-shift]
;;   (-> board
;;       (assoc :figure (new-figure rotations init-shift))
;;       (assoc :figure-rotations rotations)
;;       (assoc :figure-key figtype)))

;; (defn add-figure [board figures figtype]
;;   (add-figure-raw board (get figures figtype) figtype (:init-shift board)))

(defmulti apply-move (fn [_ mv] (cond (#{:left :right} mv) :move
                                          (= :rotate mv) :rotate
                                          true :default)))

(defmethod apply-move :move [board mv]
  (let [fig (fit-figure (:x board) (move-figure (:figure board) mv))]
    (if (check-collision (board-points board) fig)
      board
      (assoc board :figure fig))))

(defmethod apply-move :rotate [board _]
  (let [rotations (:figure-rotations board)
        fig (fit-figure (:x board) (rotate-figure rotations (:figure board)))]
    (if (check-collision (board-points board) fig)
      board
      (-> board
          (assoc :figure fig)
          (assoc :figure-rotations (next rotations))))))

(defmethod apply-move :default [board _]
  board)

(defn apply-moves [board & moves]
  (reduce apply-move board moves))

(defn board-down [{:keys [figure y figure-key] :as board}]
  (let [fig (move-figure figure :down)]
    (if (check-figure (board-points board) y fig)
      (-> board
          (add-cells figure-key figure)
          clear-figure)
      (assoc board :figure fig))))

(defn board-full? [board]
  (->> (:lines-fill board)
       keys
       (filter (partial > 0))
       empty?
       not))

(defn full-lines [lines-fill dimx]
  (->> lines-fill
       (filter #(>= (second %) dimx))
       (map first)))

(defn board-clear-full-lines [board]
  (let [fl (sort (full-lines (:lines-fill board) (:x board)))]
    ;(println fl)
    (reduce remove-line board fl)))

(defn board-cells [board]
  (conj (:cells board) (zipmap (:figure board) (repeat (:figure-key board)))))
