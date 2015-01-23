(ns tramboard-clj.core.include
  (:use [hiccup core page element]))

(defn include-javascript []
  [(include-js "//fb.me/react-0.12.2.js")
   (include-js "out/goog/base.js")
   (include-js "js/dev.js")
   ;(include-js "//code.jquery.com/jquery-2.1.3.min.js")
   ;(include-js "//netdna.bootstrapcdn.com/bootstrap/3.3.2/js/bootstrap.min.js")
   (javascript-tag "goog.require('tramboard_clj.script.tram');goog.require('tramboard_clj.script.main');goog.require('ajax.core');")])
