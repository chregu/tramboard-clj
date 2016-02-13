(ns tramboard-clj.api.bvb
  (:require [ring.util.codec :as codec]            
            [tramboard-clj.api.vblbvb :as vblbvb]
            [digest] 
          ))
          
(def query-stations-base-url        "http://efa-bw.de/ios_bvb/XML_STOPFINDER_REQUEST?coordOutputFormat=NBWT&type_sf=any&locationServerActive=1&stateless=1&useHouseNumberList=true&doNotSearchForStops=1&reducedAnyWithoutAddressObjFilter_sf=103&reducedAnyPostcodeObjFilter_sf=64&reducedAnyTooManyObjFilter_sf=2&anyObjFilter_sf=126&anyMaxSizeHitList=600&w_regPrefAl=2&prMinQu=1&name_sf=")
(def station-base-url               "http://efa-bw.de/ios_bvb/XML_DM_REQUEST?type_dm=any&trITMOTvalue100=10&changeSpeed=normal&mergeDep=1&coordOutputFormat=NBWT&coordListOutputFormat=STRING&useAllStops=1&excludedMeans=checkbox&useRealtime=1&itOptionsActive=1&canChangeMOT=0&mode=direct&ptOptionsActive=1&imparedOptionsActive=1&depType=stopEvents&locationServerActive=1&useProxFootSearch=0&maxTimeLoop=2&includeCompleteStopSeq=0&name_dm=")

(defn- get-hash [dept] 
    (str "t" (digest/md5 (str (subs (clojure.string/replace (vblbvb/map-station-name (dept :to)) "^Basel " "" ) 1 3) (dept :name)((dept :departure) :scheduled)))))


; BVB doesn't send tram colors... 
; I'm sure there's an easier way to this than with 3
;  functions, but hey, it works ;)
(defn- map-color [color dept]
(if (= (:type dept) "tram")                           
(case (:name dept)
    "1" {:fg "#FFFFFF" :bg "#845234"}
    "2" {:fg "#FFFFFF" :bg "#A7844D"}
    "3" {:fg "#FFFFFF" :bg "#304BA3"}
    "6" {:fg "#FFFFFF" :bg "#0070BF"}
    "8" {:fg "#000000" :bg "#F47AB1"}
    "10" {:fg "#000000" :bg "#FFCB00"}
    "11" {:fg "#FFFFFF" :bg "#F11714"}
    "14" {:fg "#000000" :bg "#F78200"}
    "15" {:fg "#FFFFFF" :bg "#00A64B"}
    "16" {:fg "#000000" :bg "#A5D027"}
    "17" {:fg "#FFFFFF" :bg "#00ADF2"}
    "E11" {:fg "#FFFFFF" :bg "#F11714"}
    "21" {:fg "#FFFFFF" :bg "#00AF9D"}
    {:fg "#000000" :bg "#FFFFFF"})
    
    {:fg "#000000" :bg "#FFFFFF"}
))
    

(defn- update-single-colors [dept]
    (update-in dept [:colors] map-color dept
))

(defn- update-colors [depts] 
    (map update-single-colors depts)
)
    
; TODO error handling
(defn station [id sbbid]
  (let [request-url (str station-base-url id)
        orig-return (vblbvb/station id sbbid request-url get-hash)
        with-bvb-colors (update-in orig-return [:departures ] update-colors)
  ]
      with-bvb-colors  ))

(defn query-stations [query]
  (let [request-url (str query-stations-base-url (codec/url-encode query))]
    (vblbvb/query-stations query request-url)))
