(ns dat00.osc
  (:import
   [oscP5 OscP5 OscMessage ]
   [netP5 NetAddress]))

(declare my-remote-location my-oscP5)

(def in-port 9001)

(def out-port 9000)

(defn make-osc-message [path]
  (OscMessage. path))

(defn send-osc-message [message]
  (.send my-oscP5 message my-remote-location))

(defn init-oscP5 [papplet]
  (def my-oscP5 (OscP5. papplet in-port))
  (def my-remote-location (NetAddress. "localhost" out-port)))
