(ns user
  (:require 
            [mount.core :as mount]
            arcanium.core))

(defn start []
  (mount/start-without #'arcanium.core/repl-server))

(defn stop []
  (mount/stop-except #'arcanium.core/repl-server))

(defn restart []
  (stop)
  (start))


