(ns dat00.espe
  (:use
    dat00.protocols
    quil.core
    [dat00.oscloops :as osc-loops]
    [dat00.graphics :as g]
    [dat00.antropoloops :as antropoloops]
    [dat00.bd :as bd]
    [dat00.util :as ut]
    [clojure.data.json :as json :only [read-str]]))

(def drawing (atom false))

(defn get-colors [^toxi.color.ColorRange cr t-color num-colors variance]
  (.getColors cr t-color num-colors variance))

(declare history-paths)

(defn setup []
  (g/setup-graphics)
  (g/load-resources))

(comment (defn lines-history []
  (doseq [track-path-raw history-paths]
    (let [track-path (first track-path-raw)
          color-list-path (last track-path-raw)]
      (no-fill)
      (doseq [n (range  (count  track-path))]
        (let [track-line-path (nth track-path n)
              color-line-path (.get color-list-path n)
              origen (first track-line-path)
              ox (+ 5 (:coordX origen))
              oy (:coordY origen)
              fin (second track-line-path)
              fx (:coordX fin)
              fy (:coordY fin)
              a1x (if (not= ox fx) (+ ox (-  fx ox)) (- ox 20) )
              a1y (if (not= oy fy)  (- oy (abs  (-  fy oy)) 100) (- oy 20))
              a2x (if (not= ox fx) fx (+ fx 50))
              a2y (if (not= oy fy)  fy (- oy 50))]
          (stroke (rgb color-line-path))
          (stroke-weight 1)
          (doseq [i (range 10)]
            (bezier (-  ox i) oy (- a1x i) (- a1y i)  a2x  a2y  fx fy))))))))

(def lugares-inside (atom {}))

(def lines (atom []))

(def main-count (atom 0))

(defn print-interior [c int-lines each-modulo-width [-color lugar volume] contador-lines-int]
  (let [starting (* c each-modulo-width)
        each-l-w (/ each-modulo-width (count int-lines))
        start-l (* contador-lines-int each-l-w)]
    (no-stroke)
    (fill (rgb -color)  (map-range volume 1 8 1 100))
    (triangle
      (+ starting start-l)
      (- (height) 100)
      (+ starting start-l each-l-w)
      (- (height) 100)
      (:coordX lugar)
      (:coordY lugar))
    (ellipse (:coordX lugar) (:coordY lugar)  5 5 )
    (rect (+ starting start-l) (- (height) 100)  each-l-w 100)))

(defn print-module-time [int-lines c]
  (when-not (empty? int-lines)
    (let [each-modulo-width (/ (width) (count @lines))]
      (doall (map (partial print-interior c int-lines each-modulo-width) int-lines (range))))))

(defn draw []
  (frame-rate 5)
  (g/draw-background)
  (g/draw-antropoloops-credits)
  (swap! main-count inc)
  (when (and (not-empty @antropoloops/antropo-loops) @drawing)
    (text "drawing!!" 10 175)
    (let [m (millis)
          active-loops (filter (fn [v]
                                 (let [{:keys [state]} (val v)]
                                   (= state 2 )))
                               @antropoloops/antropo-loops)
          a-l-c (count active-loops)
          int-lines (atom [])]
      (doseq [a-loop  active-loops]
        (let [active-loop (val a-loop)
              song (:song active-loop)
              lugar (:lugar active-loop)
              track (int (:track active-loop))
              posicion-x-disco (* 160 track)
              fecha (:fecha song)]
          (let [the-key (:lugar lugar)
                the-color (toxi.color.TColor/newRandom) ]
            (if-let [lugar-color  (find @lugares-inside the-key)]
              (swap! int-lines conj [(last lugar-color) lugar a-l-c (:volume active-loop)])
              (do
                (swap! lugares-inside assoc the-key the-color)
                (swap! int-lines conj [the-color lugar a-l-c (:volume active-loop)]))))
          (g/draw-album active-loop posicion-x-disco fecha)
          (g/draw-line-country active-loop posicion-x-disco lugar)
          (g/draw-abanico-country active-loop lugar tempo m))
        (swap! lines conj (shuffle @int-lines)))))
  (doall
    (map print-module-time @lines (range)))
  (when-not
    (empty? @lines)
    (let [each-module (/ (width) (count @lines))]
      (doseq [m (range (count @lines))]
        (stroke 1)
        (stroke-weight 1)
        (let [x-m (* m each-module)]
          (line x-m (height) x-m (- (height) 20))))))
  (g/draw-svg))

(defn key-press []
  (println (str "Key pressed: " (raw-key)))
  (condp = (raw-key)
    \1 (antropoloops/reset)
    \2 (antropoloops/async-request-loops-info)
    \3 (do (println "draw running")
         (reset! drawing true))
    \4 (println "println loops indexed" antropoloops/antropo-loops-indexed)
    \5 (println "print misatropolops" @antropoloops/antropo-loops)
    (println (str "no mapped key " (raw-key)))))

(defsketch juan
  :setup setup
  :draw draw
  :size [(screen-width) (screen-height)]
  :osc-event antropoloops/process-osc-event
  :key-typed key-press)

(osc-loops/init-oscP5-communication juan)

(defn -main
  "The application's main function"
  [& args]
  (println args))



