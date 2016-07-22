(defproject dat00 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [quil/quil "2.0.1-juan"]
                 [org.clojure/data.json "0.2.4"]
                 [clj-time "0.7.0"]
                 [im.chit/iroh "0.1.10"] ;se usa?
                 [com.datomic/datomic-free "0.9.4556"] ;se usa?
                 ]
  :resource-paths ["lib/gstreamer-java.jar" "lib/geomerative.jar" "lib/batikfont" "lib/video.jar" "lib/jna.jar" "lib/macosx64" "lib/oscP5.jar" "lib/colorutils.jar" "lib/toxiclibscore.jar"]
  :main dat00.espe
  :aot [dat00.espe]
  )
