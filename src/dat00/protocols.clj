(ns dat00.protocols
  (:import [geomerative RShape RG RPoint RFont]
           [processing.core PApplet ]
           [toxi.color TColor ColorList ColorRange NamedColor]
           [toxi.color.theory ColorTheoryStrategy ColorTheoryRegistry]))

(defprotocol countable
  (get-count [this]))

(extend-protocol countable
  ColorList
  (get-count [this]
    (.size this))
  clojure.lang.LazySeq
    (get-count [this]
    (.count this)))

(defmulti rgb type)

(defmethod rgb toxi.color.TColor [a]
  (.toARGB a))


