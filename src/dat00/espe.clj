(ns dat00.espe
  (:use
    quil.core
    [dat00.oscloops :as osc-loops]
    [dat00.graphics :as g]
    [dat00.antropoloops :as antropoloops]
    [clojure.data.json :as json :only [read-str]])
  ;(:gen-class)
  )

(def drawing (atom false))

(defn setup []
  (g/setup-graphics)
  (g/load-resources))

(def lugares-inside (atom {}))

(def lines (atom []))

;(def main-count (atom 0))

(defn draw []
  (frame-rate 5) ;; en processing no tengo definido framerate
  (g/draw-background)
  (g/draw-antropoloops-credits) ;;no se ven
  ;(swap! main-count inc)
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

(defsketch applet
    :setup setup
    :draw draw
    :size [(screen-width) (screen-height)]
    :osc-event antropoloops/process-osc-event
    :key-typed key-press
    :features [:exit-on-close]
  )

(osc-loops/init-oscP5-communication applet)

(defn -main [& args]
  (println args)
 )





