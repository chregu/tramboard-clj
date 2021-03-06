(ns tramboard-clj.api.zvv
  (:require [cheshire.core :as json]
            [ring.util.codec :as codec]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.string :as str]
            [org.httpkit.client :as http]
            [clojure.java.jdbc :refer :all]
            [clojure.tools.logging :as log]
            [clojure.tools.html-utils :as html]))

(def query-stations-base-url "http://online.fahrplan.zvv.ch/bin/ajax-getstop.exe/dny?start=1&tpl=suggest2json&REQ0JourneyStopsS0A=7&getstop=1&noSession=yes&REQ0JourneyStopsB=25&REQ0JourneyStopsS0G=")
(def station-base-url        "http://online.fahrplan.zvv.ch/bin/stboard.exe/dny?dirInput=&boardType=dep&start=1&tpl=stbResult2json&input=")
(def query-connections-base-url "http://transport.opendata.ch/v1/connections?limit=5&direct=1&")
(def zvv-timezone (t/time-zone-for-id "Europe/Zurich"))

(def zvv-date-formatter (f/with-zone (f/formatter "dd.MM.yy HH:mm") zvv-timezone))
(def input-datetime-formatter (f/with-zone (f/formatter "YYYY-MM-dd'T'HH:mm") zvv-timezone))
(def date-formatter (f/with-zone (f/formatter "YYYY-MM-dd") zvv-timezone))
(def date-formatter-dot (f/with-zone (f/formatter "dd.MM.YY") zvv-timezone))
(def time-formatter (f/with-zone (f/formatter "HH:mm") zvv-timezone))
(def z-date-formatter (f/with-zone (f/formatter "YYYY-MM-dd'T'HH:mm:ssZ") zvv-timezone)) ; FIX SOMERTIME

(def db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "stations.sqlite"
   })


(defn- sanitize-line [text]
  (reduce #(str/replace %1 (%2 0) (%2 1))
          text
          [["&nbsp;" " "] [#"S( )+" "S"] [#"SN( )+" "SN"] [#"IC( )+.*" "IC"] [#"IR( )+.*" "IR"] [#"Tro( )+" ""] [#"Bus( )+" ""] [#"Trm( )+" ""] ["Bus " ""] [" +" " "]]))

(defn sanitize-to [text]
  (reduce #(str/replace %1 (%2 0) (%2 1))
          text
          [[#" +\(SZU\)" ""]]))

(defn- map-category [text]
  (case text
    "icon_tram"  "tram"
    "icon_train" "train"
    "icon_bus"   "bus"
    "icon_boar"  "boat"

    ;"M"      "subway"
    ;"TX"     "taxi"
    ;"Fun"    "rack-train"
    ;"GB"     "cable-car"

    "train"))

(defn- station-limit [id]
  (case (clojure.string/replace id #"^0*" "")
    "8503000" "200"                                         ; Zürich HB
    "8507000" "200"                                         ; Bern
    "8507785" "200"                                         ; Bern Hauptbahnof
    "8500010" "200"                                         ; Basel SBB
    "22"  "200"                                             ; Basel
    "8505000" "200"                                         ; Luzern
    "100"
    )
  )

(defn- format-date [date time]
  (try (str (f/parse zvv-date-formatter (str date " " time))) (catch Exception e nil)))

(defn- zvv-date [input]
  (let [date (input "date")
        time (input "time")]
    (when (not (or (empty? date) (empty? time)))
      (format-date date time))))

(defn- zvv-to-sbb-id-uncached [id]
(let [sbbid (first (query db (str "select sbb_id from zvv_to_sbb where zvv_id = " id )))]
    (if (nil? sbbid) nil (:sbb_id sbbid))))

(def zvv-to-sbb-id
  (memoize zvv-to-sbb-id-uncached))

; TODO add 1 day to realtime if it is smaller than scheduled (scheduled 23h59 + 3min delay ...)
(defn- zvv-departure [zvv-journey]
  (let [product         (zvv-journey "product")
        main-location   (zvv-journey "mainLocation")
        color           (product "color")
        line            (or  (product "name")(product "line") )
        platform        (main-location "platform")
        attributes-bfr  (zvv-journey "attributes_bfr")
        timestamp       (zvv-date main-location)
        timestamprt     (zvv-date (main-location "realTime"))
        last-location   (last (zvv-journey "locations"))
        last-location-id ((last-location "location") "id")
        last-location-id-nr (read-string last-location-id)
        last-location-arrival (zvv-date last-location)
        ]
    {:name (sanitize-line line)
     :type (map-category (product "icon"))
     :accessible (not (empty? (filter #(contains? #{"6" "9" "NF"} (% "code")) attributes-bfr)))
     :colors {:fg (str "#" (color "fg"))
              :bg (str "#" (color "bg"))}
     :to (sanitize-to (html/xml-decode (product "direction")))
     :id (if (and (< last-location-id-nr 300000) (> last-location-id-nr 290000)) (zvv-to-sbb-id last-location-id) last-location-id ) ; zvv has sometimes it's internal ids on some stations
     :platform (if (= platform "") nil platform)
     :dt (or timestamprt timestamp)
     :departure {:scheduled timestamp
                 :realtime timestamprt}
     :source "zvv"
     :arrival {:scheduled last-location-arrival}}))

; TODO tests (=> capture some data from zvv api)
(defn transform-station-response [id]
  (fn [response-body]
    (let [data        (json/parse-string response-body)]
      {:meta {:station_id   id
              :station_name ((data "station") "name")}
       :departures (map zvv-departure (data "connections"))})))

(defn- to-coordinate [string]
  (if (nil? string) nil
    (double (/ (read-string string) 1000000))))

(defn- zvv-station [zvv-station]
  (let [id nil]
    {:id    (zvv-station "extId")
     :name  (zvv-station "value")
     :location {:lat (to-coordinate (zvv-station "ycoord")) :lng (to-coordinate (zvv-station "xcoord"))}}))

(defn transform-query-stations-response [response-body]
  (let [unparsed (reduce #(clojure.string/replace-first %1 %2 "") response-body [";SLs.showSuggestion();" "SLs.sls="])
        data     (json/parse-string unparsed)
        stations (data "suggestions")]
    {:stations (map zvv-station (remove #(or (nil? (% "extId")) (not= "1" (% "type"))) stations))}))

(defn- zvv-get-realtime [passlist deptOrArr]
    (let [prognosis (passlist "prognosis")
          prognosis-dept (prognosis deptOrArr)
          could-be-realtime  (= (passlist "realtimeAvailability") "RT_BHF")]
      (if (some? prognosis-dept)
        prognosis-dept
        (if could-be-realtime
            (passlist deptOrArr)
            nil))))

(defn- zvv-passlist-remove-same [passlist]
    (let [departure (:departure passlist )
          arrival   (:arrival passlist )]

          (if (and (= (:scheduled departure) (:scheduled arrival))
                   (= (:realtime departure)  (:realtime arrival))
          )
            (dissoc passlist :arrival)
            ; the if nil? assoc can be removed, when time for coffee 1.9.1 is release
            (if (nil? (:scheduled departure))
                (assoc passlist :departure arrival)
                passlist
                )
            )))

(defn- zvv-passlist [passlist]
    (let [station   (passlist "station")
          departure (passlist "departure")
          arrival   (passlist "arrival")
          coord  (station "coordinate")]
    {:name (station "name")
     :id (clojure.string/replace (station "id") #"^0*" "")
     :location {:lat (coord "x")
                :lng (coord "y")}
     :departure {:scheduled departure
                 :realtime (zvv-get-realtime passlist "departure")}
     :arrival {:scheduled arrival
                 :realtime (zvv-get-realtime passlist "arrival")}
                 }))

(defn- zvv-section [section]
    (let [passlist (map zvv-passlist ((section "journey") "passList"))]
     (map zvv-passlist-remove-same passlist)))

(defn- zvv-connection [connection]
    (let [sections (->> (connection "sections")
                        (filter #(not (nil? (% "journey"))))
                        (map zvv-section))]
       (first sections)))

(defn transform-query-connections-response [date arrivaldate]
  (fn [response-body]
  (let [data     (json/parse-string response-body)
        got-useful-response (some? data)
        connections-all (if got-useful-response
                          (->> (data "connections")
                             (map zvv-connection))
                             [])

        connections  (->> connections-all
                         (filter #(if (= (:scheduled (:departure (first %))) date) true false)))

        ; If we have more than one connection with the same departure time, filter
        ; according to the arrival time if we have that (example is "Zurich -> Zug" where
        ; 2 trains start at xx:04 to Zug, the S9 and an IR
        connections-arrival-filtered (if (and (some? arrivaldate) (> (count connections) 1))
                                       (filter #(if (= (:scheduled (:departure (last %))) arrivaldate) true false) connections) connections)
        ; If we couldn't find a match with arrivaltime, just return the inital connections
        ;  Better this than nothing
        connections-final (if (= (count connections-arrival-filtered) 0)
                            connections
                            connections-arrival-filtered)
        ; If we couldn't find any matching departure, just take the one nearest to the time and in the past
        ;  gva doesn't return scheduled times, therefore we need this here to at least show something
        connections-final2 (if (= (count connections-final) 0)
                            (->> connections-all
                                ; filter out all departures later
                                (filter #(if (> (compare (:scheduled (:departure (first %))) date) 0) false true))
                                last
                                )
                            connections-final)
                            ]
    {:passlist connections-final2})))

; TODO error handling
(defn- do-api-call [url transform-fn]
  (let [response (http/get url)
        status (:status @response)]
    (if (= status 200)
      (transform-fn (:body @response))
      {:error "error"
       :status status
       :url url})))

(defn zvv-station-url [id]
  (str station-base-url id "&maxJourneys=" (station-limit id)))

(defn station [id sbbid]
  (let [request-url (zvv-station-url sbbid)]
  (do-api-call request-url (transform-station-response sbbid))))

(defn station-with-time [id sbbid datetime]
  (let [request-url (zvv-station-url id)
        datetime2 (f/parse input-datetime-formatter datetime)
        request-url-with-time (str request-url "&date=" (codec/url-encode (f/unparse date-formatter-dot datetime2)) "&time=" (codec/url-encode (f/unparse time-formatter datetime2)))]
    (do-api-call request-url-with-time (transform-station-response id))))

(defn query-stations [query]
  (let [request-url (str query-stations-base-url (codec/url-encode query))]
    (do-api-call request-url transform-query-stations-response)))

(defn query-connections [from to datetime]
  (let [date (f/parse input-datetime-formatter datetime)
        date-10 (t/plus date (t/minutes -10))
        request-url (str query-connections-base-url "from=" (codec/url-encode from) "&to=" (codec/url-encode to) "&date=" (codec/url-encode (f/unparse date-formatter date-10)) "&time=" (codec/url-encode (f/unparse time-formatter date-10)))]
;{:url request-url}))
    (do-api-call request-url (transform-query-connections-response (f/unparse z-date-formatter date) nil))))

(defn query-connections-with-arrival [from to datetime arrivaltime]
  (let [date (f/parse input-datetime-formatter datetime)
        arrivaldate (f/parse input-datetime-formatter arrivaltime)
        date-10 (t/plus date (t/minutes -10))
        request-url (str query-connections-base-url "from=" (codec/url-encode from) "&to=" (codec/url-encode to) "&date=" (codec/url-encode (f/unparse date-formatter date-10)) "&time=" (codec/url-encode (f/unparse time-formatter date-10)))]
; {:url request-url }))
    (do-api-call request-url (transform-query-connections-response (f/unparse z-date-formatter date) (f/unparse z-date-formatter arrivaldate)))))



