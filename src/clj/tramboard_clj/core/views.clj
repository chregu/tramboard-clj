(ns tramboard-clj.core.views
  (:use [hiccup core page element]
        [tramboard-clj.core.include])
  (:require [tramboard-clj.api.gva]
            [tramboard-clj.api.odp]
            [tramboard-clj.api.zvv]
            [tramboard-clj.api.vbl]
            [tramboard-clj.api.bvb]
            [tramboard-clj.api.blt]
            [tramboard-clj.api.ost]
            [clojure.java.jdbc :refer :all]
            )
  (:import com.newrelic.api.agent.Trace))

(def fallback-api "zvv")

(def db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "stations.sqlite"
   })

(defn- content-page [content]
  (let [description "Real-time departures of public transport in Switzerland, for bus, train, tram, cable car..."
        lang        "en"
        title       "Time for Coffee!"]
    (html5 {:lang lang}
           [:head {:profile "http://www.w3.org/2005/10/profile"}
            [:meta {:charset "UTF-8"}]
            [:meta {:http-equiv "Content-Type"    :content "text/html;charset=utf-8"}]
            [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
            [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
            [:meta {:name "language" :content lang}]
            [:meta {:name "og:locale" :content lang}]

            [:meta {:name "description" :content description}]
            [:meta {:name "og:description" :content description}]

            [:title "Time for Coffee!"]
            [:meta {:name "og:title" :content title}]
            [:meta {:name "og:url" :content "http://www.timeforcoffee.ch/"}]

            [:link {:rel "icon" :type "image/png" :href "/favicon.png"}]

            (include-css "//netdna.bootstrapcdn.com/bootstrap/3.3.2/css/bootstrap.min.css")
            (include-css "//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css")
            (include-css "/css/styles.css")
            (javascript-tag (str "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                                 (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                                 m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                                 })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
                                 ga('create', '" (or (System/getenv "GA_TRACKING") "dummy") "', 'auto');ga('send', 'pageview');"))
            (com.newrelic.api.agent.NewRelic/getBrowserTimingHeader)]
           (into
             [:body
              content
              (com.newrelic.api.agent.NewRelic/getBrowserTimingFooter)]
             (include-javascript)
             ))))

(defn- index-page* []
  (content-page [:div {:id "my-app"}]))

(defn- about-page* []
  (content-page
    [:div
     [:header {:class "menu-bar"}
      [:div {:class "container-fluid"}
       [:div {:class "menu-icon-parent"}
        [:span {:class "left-icon"}
         [:a {:class "link-icon glyphicon glyphicon-home" :href "/" :aria-label "go back"}]]
        [:span {:class "text-middle bold"} "You've got Time for Coffee!"]]]]
     [:div {:class "container-fluid"}
      [:h1 {:class "ultra-thin text-center"} "Contributors"]
      [:h3 {:class "ultra-thin text-center"} [:a {:href "http://twitter.com/fterrier"} "François Terrier"] " - original idea, API and web programming"]
      [:h3 {:class "ultra-thin text-center"} [:a {:href "http://twitter.com/jlgeering"} "Jean-Luc Geering"] " - original idea, API programming"]
      [:h3 {:class "ultra-thin text-center"} [:a {:href "http://twitter.com/serge_pfeifer"} "Serge Pfeifer"] " - original idea, testing"]
      [:h3 {:class "ultra-thin text-center"} [:a {:href "http://twitter.com/dulnan"} "Jan Hug"] " - API, iOS app UX, design and programming"]
      [:h3 {:class "ultra-thin text-center"} "Kristina Bagdonaite" " - web UX"]
      [:h3 {:class "ultra-thin text-center"} [:a {:href "http://twitter.com/chregu"} "Christian Stocker"] " - iOS app & Apple watch programming"]
      [:h3 {:class "ultra-thin text-center"} [:a {:href "http://twitter.com/gabac"} "Cyril Gabathuler"] " - iOS app & Apple watch programming"]

      [:h1 {:class "ultra-thin text-center"} "Special Thanks"]
      [:h3 {:class "ultra-thin text-center"} [:a {:href "http://transport.opendata.ch/"} "OpenData Transport API"] " / " [:a {:href "http://www.liip.ch/"} "Liip AG"]]

      [:h2 {:class "thin text-center"} [:a {:href "http://twitter.com/time4coffeeApp"} "Get in touch "] " & " [:a {:href "http://github.com/timeforcoffee/"} "contribute!"] ]]]))



(defn- get-api-key-uncached [id]
  (let [stripped_id (clojure.string/replace id #"^0*" "")]
  (first (query db (str "select zapikey as apikey, zapiid as apiid from ZTFCSTATIONMODEL where ZID = " stripped_id)))))

(def get-api-key
  (memoize get-api-key-uncached))

(defn- apikey-lookup-in-db [api id]
    (let [apikeys-result (get-api-key id)
          apikey (if (or (nil? apikeys-result) (nil? (apikeys-result :apikey))) fallback-api (apikeys-result :apikey))
          apiid (if (or (nil? apikeys-result) (nil? (apikeys-result :apiid))) id (apikeys-result :apiid))
          apikeys {:apikey apikey :apiid apiid}
    ]
    apikeys))

(defn- apikey-lookup [api id]
    (if (= api "ch") (apikey-lookup-in-db api id) {:apikey api :apiid id}))

(defn- sbb-id-lookup [api id]
    (if (= api "ch") id id))

(defn station* [api id]
  (let [id2 (clojure.string/replace id #"^0*" "")
        apikey (apikey-lookup api id2)

  ]
  {:headers {"Content-Type" "application/json; charset=utf-8" "TFCSource" (apikey :apikey)}
   :body ((resolve (symbol (str "tramboard-clj.api." (apikey :apikey) "/station"))) (apikey :apiid) (sbb-id-lookup api id2))}))

(defn station-with-time* [api id datetime]
  (let [id2 (clojure.string/replace id #"^0*" "")
        apikey (apikey-lookup api id2)]
  {:headers {"Content-Type" "application/json; charset=utf-8"}
   :body ((resolve (symbol (str "tramboard-clj.api." (apikey :apikey) "/station-with-time"))) (apikey :apiid) (sbb-id-lookup api id2) datetime)}))

(defn- query-stations* [api query]
  (let [apikey (if (= api "ch") fallback-api api)]
  {:headers {"Content-Type" "application/json; charset=utf-8"}
   :body ((resolve (symbol (str "tramboard-clj.api." apikey "/query-stations"))) query)}))

(defn- query-connections* [api from to datetime]
  (let [apikey (if (= api "ch") fallback-api api)]
  {:headers {"Content-Type" "application/json; charset=utf-8"}
   :body ((resolve (symbol (str "tramboard-clj.api." apikey "/query-connections"))) from to datetime)}))

(defn- query-connections-with-arrival* [api from to datetime arrivaltime]
  (let [apikey (if (= api "ch") fallback-api api)]
    {:headers {"Content-Type" "application/json; charset=utf-8"}
     :body ((resolve (symbol (str "tramboard-clj.api." apikey "/query-connections-with-arrival"))) from to datetime arrivaltime)}))

(definterface INR
  (indexPage        [])
  (aboutPage        [])
  (station          [api id])
  (stationWithTime  [api id datetime])
  (queryStations    [api query])
  (queryConnections [api from to datetime])
  (queryConnectionsWithArrival [api from to datetime arrivaltime]))

(deftype NR []
  INR
  ;; @Trace maps to Trace {} metadata:
  (^{Trace {}} indexPage        [_]       (index-page*))
  (^{Trace {}} aboutPage        [_]       (about-page*))
  (^{Trace {}} station          [_ api id]           (station* api id))
  (^{Trace {}} stationWithTime  [_ api id datetime]  (station-with-time* api id datetime))
  (^{Trace {}} queryStations    [_ api query]        (query-stations* api query))
  (^{Trace {}} queryConnections [_ api from to datetime] (query-connections* api from to datetime))
  (^{Trace {}} queryConnectionsWithArrival [_ api from to datetime arrivaltime] (query-connections-with-arrival* api from to datetime arrivaltime)))

(def ^:private nr (NR.))

(defn index-page        []                 (.indexPage nr))
(defn about-page        []                 (.aboutPage nr))
(defn station           [api id]           (.station nr api id))
(defn station-with-time [api id datetime]  (.stationWithTime nr api id datetime))
(defn query-stations    [api query]        (.queryStations nr api query))
(defn query-connections [api from to datetime] (.queryConnections nr api from to datetime))
(defn query-connections-with-arrival [api from to datetime arrivaltime] (.queryConnectionsWithArrival nr api from to datetime arrivaltime))
