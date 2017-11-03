(ns arcanium.routes.home
  (:require [arcanium.layout :as layout]
            [compojure.core :refer [defroutes POST GET]]
            [clojure.core.match :refer [match]]
            [ring.util.http-response :as response]
            [clojure.tools.logging :as log]
            [ring.util.http-response :refer [found content-type ok]]
            [clojure.java.io :as io]))

(defn home-page []
  (layout/render
    "home.html" {:docs (-> "docs/docs.md" io/resource slurp)}))

(defn about-page []
  (layout/render "about.html"))

(defn bangRegex [bang]
  (re-pattern (str "(^|\\s)" bang "($|\\s)")))

(defn bangFinder [bang query] 
  (re-find (bangRegex bang) query))

(defn bangFinder [bang query] 
  (re-find (bangRegex bang) query))

(defn bangReplacer [bang query] 
  (clojure.string/replace query (bangRegex bang) ""))

(defn findBangs [query]
  (match [query]
         [_ :guard #(bangFinder "!g" %)] (str "https://google.com/search?q=" (bangReplacer "!g" query))
         [_ :guard #(bangFinder "!b" %)] (str "https://bing.com/search?q=" (bangReplacer "!b" query))
         :else nil))

(defn search-page [req]
  (log/debug "test")
  (log/debug req)
  (found (findBangs (get-in req [:params :q]))) )

(defn test-page []
  (content-type
    (ok
      (-> "frontend/1.html" io/resource slurp))
    "text/html; charset=utf-8"))

(defroutes home-routes
  (GET "/" [] (home-page))
  (GET "/search" req (search-page req))
  (GET "/test" [] (test-page))
  (GET "/about" [] (about-page)))

