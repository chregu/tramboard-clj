(ns tramboard-clj.api.odp
  (:require [ring.util.codec :as codec]
            [digest]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [cheshire.core :as json]
            [org.httpkit.client :as http]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :refer [text xml-> xml1->]]
            [tramboard-clj.api.zvv :as zvv]
            [tramboard-clj.api.common :as c]))

(def station-base-url               "https://api.opentransportdata.swiss/trias")


(def odp-timezone (t/time-zone-for-id "Europe/Zurich"))
(def odp-date-formatter (f/with-zone (f/formatter "YYYY-MM-dd'T'HH:mm:ssZ") odp-timezone))

(defn- odp-parse-datetime [timestamp]
  (if (or (nil? timestamp) (= "" timestamp))
    nil
    (try (str (f/parse odp-date-formatter timestamp)) (catch Exception e nil))))

(defn zip-str [s]
  (zip/xml-zip 
    (try
      (xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))
      (catch Exception e {:error"xml parse error"}))))  


; if the hash making fails due to too different names, fix it here
(defn map-station-name [text]
  (case text
    "Basel St-Louis Grenze" "St-Louis Grenze"
    text))

(defn- map-category [text]
  (case text
    "Bus" "bus"
    "S-Bahn" "train"
    "InterCityNeigezug" "train"
    "Regional-Express" "train"
    "Voralpen-Express" "train"
    "InterCityExpress" "train"
    "InterCityNight" "train"
    "EuroCity" "train"
    "InterCity" "train"
    "InterRegio" "train"
    "Trolleybus" "bus"
    "Schiff" "ship"
    "Tram BLT" "tram"
    "Tram BVB" "tram"
    "Bus BVB" "bus"
    "train"))

(defn- name-category [text default]
  (case text
    "InterCityNeigezug" "ICN"
    "InterCityNight" "ICN"
    "InterCityExpress" "ICE"
    "Regional-Express" "RE"
    "InterRegio" "IR"
    "Voralpen-Express" "VAE"
    "EuroCity" "EC"
    "InterCity" "IC"
    (if (= default "") text default)))    

(defn- odp-departure [odp-journey]
  (let [timestamp  (odp-parse-datetime (xml1-> odp-journey  :ThisCall :CallAtStop :ServiceDeparture :TimetabledTime text) )
        timestamprt
        (if (= "0" (xml1-> odp-journey  :ThisCall :CallAtStop :ServiceDeparture :EstimatedTime text))
          nil
          (odp-parse-datetime (xml1-> odp-journey  :ThisCall :CallAtStop :ServiceDeparture :EstimatedTime text))
          )
        line-code  (xml1-> odp-journey :m :nu text)
        line-code-type (xml1-> odp-journey :m :n text)
        line-color "#000000"
        name  (xml1-> odp-journey  :Service :PublishedLineName :Text text)
        ]
    {:type (map-category (xml1-> odp-journey  :Service :Mode :Name :Text text))
     :name (if (= (xml1-> odp-journey  :Service :Mode :RailSubmode text) "suburbanRailway")
      (str "S" name) name)
     :accessible false
     :colors {:fg "#000000" :bg "#FFFFFF"}
     :to (xml1-> odp-journey  :Service :DestinationText :Text text)
     :platform nil ; Not yet available
     :dt (or timestamprt timestamp)
     :source "odp"
     :departure {:scheduled timestamp
                 :realtime timestamprt}}))

; TODO tests (=> capture some data from zvv api)
(defn transform-station-response [id]
  (fn [response-body]
     (let [data           (zip-str response-body)
            journeys       (xml-> data  :ServiceDelivery :DeliveryPayload :StopEventResponse :StopEventResult :StopEvent)
            station        (xml1-> data :ServiceDelivery :DeliveryPayload :StopEventResponse :StopEventResult :StopEvent :ThisCall :CallAtStop)]
        {:meta (if (nil? station)
                {:station_id id}
                {:station_id (xml1-> station :StopPointRef  text )
                 :station_name (xml1-> station :StopPointName :Text text )})
         :departures (map odp-departure  journeys)}
        )))

(defn- odp-station [odp-station]
  {:id    (xml1-> odp-station :r :id text )
   :name  (str (str (xml1-> odp-station :r :pc text ) ", ") (xml1-> odp-station :n text ))})

(defn- transform-query-stations-response [response-body]
  (let [stations (zip-str (clojure.string/replace  response-body "encoding=\"ISO-8859-1\"" ""))]
    {:stations (map odp-station (xml-> stations :sf :p))}))

(defn postrequestParams [id]
    {:body (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Trias version=\"1.1\" xmlns=\"http://www.vdv.de/trias\" xmlns:siri=\"http://www.siri.org.uk/siri\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
          <ServiceRequest>
              <siri:RequestTimestamp>2016-12-26T20:47:00</siri:RequestTimestamp>
              <siri:RequestorRef>Time for Coffee</siri:RequestorRef>
              <RequestPayload>
                  <StopEventRequest>
                      <Location>
                          <LocationRef>
                              <StopPointRef>" id "</StopPointRef>
                          </LocationRef>
                      </Location>
                      <Params>
                          <NumberOfResults>40</NumberOfResults>
                          <StopEventType>departure</StopEventType>
                          <IncludePreviousCalls>false</IncludePreviousCalls>
                          <IncludeOnwardCalls>false</IncludeOnwardCalls>
                          <IncludeRealtimeData>true</IncludeRealtimeData>
                      </Params>
                  </StopEventRequest>
              </RequestPayload>
          </ServiceRequest></Trias>")
    :headers {
        "Authorization" "57c5dbbbf1fe4d00010000187f09dc4f841545f96c36966cd046d71d"
        "Content-type" "text/xml"
    }
    :insecure? true
    :socket-timeout 5000
    :conn-timeout 3000})

(defn- odp-do-api-call [url transform-fn id]
  (let [response   (http/post url (postrequestParams id))]
  (transform-fn (:body @response))
 ))

; TODO error handling
(defn- get-hash [dept]
    (str "t" (digest/md5 (str (subs (dept :to) 1 3) (dept :name)((dept :departure) :scheduled)))))

(defn odp-do-api-call2 [url transform-fn url2 transform-fn2 get-hash id]
  (let [response    (http/post url (postrequestParams id))
        response2   (http/get url2 {:socket-timeout 5000 :conn-timeout 3000})]
    (c/combine-results (if (= 200 (:status @response)) (transform-fn (:body @response)) {:error (:status @response)}) (transform-fn2 (:body @response2)) get-hash)))

(defn odp-station [id sbbid request-url get-hash]
  (let [sbbid2 (if (nil? sbbid) id sbbid)
        request-url-sbb (zvv/zvv-station-url sbbid2)]
      (odp-do-api-call2 request-url (transform-station-response id)  request-url-sbb (zvv/transform-station-response sbbid2) get-hash id)))

; To ask on opentransportdata.swiss AND zvv (since zvv eg has platform data)
(defn station [id sbbid]
  (let [request-url station-base-url]
        (odp-station id sbbid request-url get-hash)))

; to only ask on opentransportdata.swiss
(defn station_ [id sbbid]
  (let [request-url station-base-url]
      (odp-do-api-call request-url (transform-station-response id) id )))


