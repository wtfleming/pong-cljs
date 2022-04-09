(ns pong.browser
  (:require
   [goog.dom :as dom]
   [goog.events :as events]))

(defn handle-click [event]
  (js/console.log "button pressed"))


;; start is called by init and after code reloading finishes
(defn ^:dev/after-load start []
  (js/console.log "start")
  (events/listen
   (dom/getElement "button")
   (.-CLICK events/EventType)
   handle-click)

  )

(defn init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (js/console.log "init")
  (start))
;;
;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (js/console.log "stop")
  (events/unlisten
   (dom/getElement "button")
   (.-CLICK events/EventType)
   handle-click))



(defn foo []
  (-> js/document
      (.getElementById "app")
      (.-innerHTML)
      (set! "Hello Clojure!!")))


(comment
  (js/console.log "foo")
  (foo)
  )

