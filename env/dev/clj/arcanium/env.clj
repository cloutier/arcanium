(ns arcanium.env
  (:require [selmer.parser :as parser]
            [clojure.tools.logging :as log]
            [arcanium.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[arcanium started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[arcanium has shut down successfully]=-"))
   :middleware wrap-dev})
