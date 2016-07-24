(ns dat00.oscloops
(:import
     [oscP5 OscP5 OscMessage ]
     [netP5 NetAddress Logger])
(:use
   quil.core
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

(defn substring? [sub st]
  (if (not= (.indexOf st sub) -1)
    true
    false))

(defn event-to-keyword [message]
  (let [path (.addrPattern message)]
   (condp substring?  path
     "/live/name/clip" :clip
     "/live/clip/info" :info
     "/live/play" :play
     "/live/clip/loopend" :loopend
     "/live/volume" :volume
     "/live/solo" :solo
     "/live/tempo" :tempo
     (do (println "OSC-EVENT NOT FILTERED" path)))))

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

(defn read-clip-info [osc-message]
  (let [[track clip nombre] (.arguments osc-message)]
    {:track track :clip clip :nombre nombre}))

(defn init-oscP5-communication [papplet]
  (osc/init-oscP5 papplet))

