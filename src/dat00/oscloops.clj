(ns dat00.oscloops
(:import
     [oscP5 OscP5 OscMessage ]
     [netP5 NetAddress Logger])
(:use
   quil.core
   [dat00.util :as util]
   [dat00.osc :as osc]))

(defn make-call-bis [namee & more]
  (if (nil? (first more))
    (list (symbol (str "." (name namee))))
    (concat (list (symbol (str "." (name namee)))) more)))

(defmacro map-get [class  things]
  `(-> ~class (~@(make-call-bis (first things) (second things) ))))


(defmacro map-direct-get [class things-col]
  (let [res (reduce (fn [c things]
                      (apply assoc c [(first things)
                                      `(let [o# (map-get ~class [:get (second ~things)])]
                                         (map-get o# [~@(vector (last things))]))]))
                    {}
                    things-col)]
    (if  (> (count res) 1)
      res
      (do
        (println res)
        (val (first res)))
      )))

(defn event-to-keyword [message]
  (let [path (.addrPattern message)]
   (condp util/substring?  path
     "/live/name/clip" :clip
     "/live/clip/info" :info
     "/live/play" :play
     "/live/clip/loopend" :loopend
     "/live/volume" :volume
     "/live/solo" :solo
     "/live/tempo" :tempo)))

(defn make-async-call-for-loop [{:keys [clip track]}]
  (-> (osc/make-osc-message "/live/clip/loopend")
      (.add (int-array [track clip]) )
      (osc/send-osc-message))
  (-> (osc/make-osc-message "/live/volume")
      (.add  track )
      (osc/send-osc-message))
  (-> (osc/make-osc-message "/live/solo")
      (.add  track )
      (osc/send-osc-message))
  (-> (osc/make-osc-message "/live/tempo")
      (osc/send-osc-message)))

(defn make-async-call-for-all-clips []
  (osc/send-osc-message (osc/make-osc-message "/live/name/clip")))

(defn read-clip-info [osc-message ]
  (let [[track clip nombre] (.arguments osc-message)]
    {:track track :clip clip :nombre nombre}))

(defn init-oscP5-communication [papplet]
  (osc/init-oscP5 papplet))

(defn model-related [m]
  (condp = m
    :loopend [ [:loopend-value 0 :floatValue]]
    :info [ [:track-value 0 :intValue] [:clip-value 1 :intValue] [:state-value 1 :intValue]]
    :jo [ [:pepe 0 :intValue] [:jose 1 :stringValue]]
    nil
    ))
