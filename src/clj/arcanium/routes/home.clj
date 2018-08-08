(ns arcanium.routes.home
  (:require [arcanium.layout :as layout]
            [compojure.core :refer [defroutes POST GET]]
            [clojure.core.match :refer [match]]
            [ring.util.http-response :as response]
            [clojure.tools.logging :as log]
            [clojure.data.json :as json]
            [clj-http.client :as client]
            [ring.util.http-response :refer [found content-type ok]]
            [clojure.java.io :as io]))

(defn doc-page []
  (layout/render
    "home.html" {:docs (-> "docs/docs.md" io/resource slurp)}))

(defn about-page []
  (layout/render "about.html"))

(defn suggest-json [req]
  (log/debug (type (:body (client/get
       "https://www.wikidata.org/w/api.php?action=wbsearchentities&search=test&format=json&language=en&uselang=en&type=item"
       {:accept :json
        :cookie-policy :standard}))))
  (let [wikidata-req
        (:body (client/get
                "https://www.wikidata.org/w/api.php?action=wbsearchentities&search=test&format=json&language=en&uselang=en&type=item"
                {:accept :json
                 :cookie-policy :standard}))
        result-json (json/read-str wikidata-req)
        only-string (map #(get-in % ["label"]) (get-in result-json ["search"]))
        ]
    (ok only-string)))

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
         :else (str "https://google.com/search?q=" query)))

(defn search-page [req]
  (log/debug "test")
  (log/debug req)
  (found (findBangs (get-in req [:params :q]))) )

(defn home-page []
  (content-type
    (ok
      (-> "pages/home.html" io/resource slurp))
    "text/html; charset=utf-8"))

(defroutes home-routes
  (GET "/" [] (home-page))
  (GET "/search" req (search-page req))
  (GET "/suggest" req (suggest-json req))
  (GET "/doc" [] (doc-page))
  (GET "/about" [] (about-page)))

