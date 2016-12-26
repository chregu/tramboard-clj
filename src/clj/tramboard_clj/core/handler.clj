(ns tramboard-clj.core.handler
  (:require [environ.core :refer [env]]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults secure-site-defaults api-defaults]]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [ring.middleware.json :refer [wrap-json-response]]
            [ring.middleware.etag :refer [wrap-etag]]
            [tramboard-clj.core.views :refer :all]))

(def env-site-defaults
  (if (= (env :https) "true")
    (assoc secure-site-defaults :proxy true)
    site-defaults))

(defn wrap-no-cache [h]
  (fn [req]
    (let [resp (h req)]
      (assoc-in resp [:headers "cache-control"] "no-cache"))))

(defn wrap-cache [h]
  (fn [req]
    (let [resp (h req)]
      (assoc-in resp [:headers "cache-control"] "public"))))

(defn wrap-cache-10-sec [h]
  (fn [req]
    (let [resp (h req)]
      (assoc-in resp [:headers "cache-control"] "public, max-age=20"))))

(defn wrap-error [h]
  (fn [req]
    (let [resp (h req)]
      (if (contains? (:body resp) :status)
            (assoc-in resp [:status] ((:body resp) :status))
            resp))))

(defroutes api-routes
  (context "/api" []
    (wrap-routes (wrap-routes (GET "/:api/stationboard/:id{.+}/:datetime{.+}" [api id datetime] (station-with-time api id datetime)) wrap-json-response) wrap-cache-10-sec)
    (wrap-routes (wrap-routes (GET "/:api/stationboard/:id{[^/]+}" [api id] (station api id)) wrap-json-response) wrap-cache-10-sec)
    (wrap-routes (wrap-routes (wrap-routes (GET "/:api/connections/:from{.+}/:to{.+}/:datetime{.+}/:arrivaltime{.+}" [api from to datetime arrivaltime] (query-connections-with-arrival api from to datetime arrivaltime)) wrap-json-response) wrap-cache-10-sec) wrap-error)
    (wrap-routes (wrap-routes (wrap-routes (GET "/:api/connections/:from{.+}/:to{[^/]+}/:datetime{.+}" [api from to datetime] (query-connections api from to datetime)) wrap-json-response) wrap-cache-10-sec) wrap-error)
    (wrap-routes (wrap-routes (GET "/:api/stations/:query{.+}" [api query] (query-stations api query)) wrap-json-response) wrap-cache)))

(defroutes app-routes
  (wrap-routes (GET "/"      [] (index-page)) wrap-cache)
  (wrap-routes (GET "/about" [] (about-page)) wrap-cache)
  (wrap-routes (route/resources "/public") wrap-cache)
  (route/not-found "404"))

(def site
  (wrap-routes app-routes wrap-defaults (assoc env-site-defaults :cookies false :session false :security (assoc (env-site-defaults :security) :frame-options false))))

(def api
  (wrap-routes api-routes wrap-defaults api-defaults))

(def app
  (wrap-gzip (wrap-etag (routes api site))))
