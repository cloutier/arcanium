(ns arcanium.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[arcanium started successfully]=-"))
   :stop
   (fn []
     (log/info "\n-=[arcanium has shut down successfully]=-"))
   :middleware identity})
