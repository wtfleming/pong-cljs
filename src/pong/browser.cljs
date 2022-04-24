(ns pong.browser
  (:require
   [goog.dom :as dom]
   [goog.events :as events]))

(def the-canvas (dom/getElement "canvas"))

(defrecord Ball [x y dx dy speed radius color])
(defrecord Paddle [x y width height speed])
(defrecord State [last-frame-time animation-frame-id ball player-one-score player-two-score player-one-paddle player-two-paddle playing?])

(defn mk-paddle
  "Creates a paddle at given x,y coordinates"
  [x y]
  (Paddle. x y 15 74 150))

(defn degrees->radians
  "Converts an angle in degrees to radians"
  [degree]
  (* degree (/ Math/PI 180)))

(defn degrees->heading
  "Converts degrees to a unit vector pointing in degrees direction.
   See https://math.stackexchange.com/questions/180874/convert-angle-radians-to-a-heading-vector"
  [degrees]
  (let [radians (degrees->radians degrees)]
    [(Math/cos radians) (Math/sin radians)]))

(defn random-in-range
  "Returns a random floating point number between min and max.
   See https://stackoverflow.com/questions/40431966/what-is-the-best-way-to-generate-a-random-float-value-included-into-a-specified"
  [min max]
  (+ min (* (rand) (- max min))))

(defn serve-direction
  "Returns a 2D unit vector representing the direction
   the ball will travel when it is served or hits a paddle"
  [min-angle-degrees max-angle-degrees]
  (-> (random-in-range min-angle-degrees max-angle-degrees)
      (degrees->heading)))

;; 0 degrees is pointing right
;; 90 is down
;; 180 is left
;; 270 is up
(defn player-one-serve-direction []
  (serve-direction -60 60))

(defn player-two-serve-direction []
  (serve-direction 120 240))

(defn mk-ball [player-serving]
  (let [x (if (= :player-one player-serving) 50 600)
        [dx dy] (if (= :player-one player-serving)
                  (player-one-serve-direction)
                  (player-two-serve-direction))]
    (Ball. x 250 dx dy 300 10 "#0000ff")))

(defn mk-state []
  (map->State  {:last-frame-time (.getTime (js/Date.))
                :animation-frame-id nil
                :ball (mk-ball :player-one)
                :player-one-score 0
                :player-two-score 0
                :player-one-paddle (mk-paddle 15 213)
                :player-two-paddle (mk-paddle 670 213)
                :playing? false}))

(def state (atom (mk-state)))

(defn colliding?
  "Returns true if the ball and paddle are colliding, otherwise returns false"
  [ball paddle]
  (let [cx (:x ball)
        cy (:y ball)
        radius (:radius ball)
        rx (:x paddle)
        ry (:y paddle)
        rw (:width paddle)
        rh (:height paddle)
        test-x (if (< cx rx)
                 rx ;; test the left edge
                 (if (> cx (+ rx rw))
                   (+ rx rw) ;; test the right edge
                   cx ;; between edges, so use the center of the circle
                   ))
        test-y (if (< cy ry)
                 ry ;; test the top edge
                 (if (> cy (+ ry rh))
                   (+ ry rh) ;; test the bottom edge
                   cy ;; between edges, so use the center of the circle
                   ))
        dist-x (- cx test-x)
        dist-y (- cy test-y)
        ;; Distance (via the Pythagorean theorem) from edge of the rectangle to
        ;;the center of the circle, squared.
        dist-squared (+ (* dist-x dist-x) (* dist-y dist-y))]

    ;; Square the radius so we don't have to do a relatively
    ;; expensive square root of dist-squared
    (<= dist-squared (* radius radius))))

(defn update-ball-after-paddle-collision!
  [new-x new-dx new-dy]
  (let [ball (:ball @state)]
    (swap! state assoc-in [:ball :x] new-x)

    ;; Increase it's speed
    (swap! state assoc-in [:ball :speed] (+ 20 (:speed ball)))

    ;; Change the angle the ball is moving
    (swap! state assoc-in [:ball :dx] new-dx)
    (swap! state assoc-in [:ball :dy] new-dy)))

(defn handle-collisions! [canvas-height]
  (let [ball (:ball @state)
        {:keys [radius y dy]} ball
        ball-top-y (- y radius)
        ball-bottom-y (+ y radius)
        player-one-paddle (:player-one-paddle @state)
        player-two-paddle (:player-two-paddle @state)]

    (when (colliding? ball player-one-paddle)
      ;; Move the ball to the right of the player one paddle, and re-serve
      (let [new-x (+ (:x player-one-paddle) (:width player-one-paddle) radius 0.001)
            [new-dx new-dy] (player-one-serve-direction)]
        (update-ball-after-paddle-collision! new-x new-dx new-dy)))

    (when (colliding? ball player-two-paddle)
      ;; Move the ball to the left of the player two paddle, and re-serve
      (let [new-x (- (:x player-two-paddle) radius 0.001)
            [new-dx new-dy] (player-two-serve-direction)]
        (update-ball-after-paddle-collision! new-x new-dx new-dy)))

    ;; If the ball has moved past the top or bottom bounds of the screen,
    ;; move it back to the edge of the screen.
    ;; Add additional value to avoid floating point comparison problems
    (when (< ball-top-y 0)
      (swap! state assoc-in [:ball :y] (+ radius 0.001))
      (swap! state assoc-in [:ball :dy] (* -1 dy)))

    (when (>= ball-bottom-y canvas-height)
      (swap! state assoc-in [:ball :y] (- canvas-height radius 0.001))
      (swap! state assoc-in [:ball :dy] (* -1 dy)))))

(defn player-one-scored?
  "Is the ball past the right edge of the play field?"
  [canvas-width]
  (let [{:keys [x radius]} (:ball @state)
        right-side-of-ball (+ x radius)]
    (> right-side-of-ball canvas-width)))

(defn player-two-scored?
  "Is the ball past the left edge of the play field?"
  []
  (let [{:keys [x radius]} (:ball @state)
        left-side-of-ball (- x radius)]
    (< left-side-of-ball 0)))

(defn update-paddle-one! [dt]
  (let [ball (:ball @state)
        paddle (:player-one-paddle @state)

        paddle-half-height (/ (:height paddle) 2)
        middle-of-screen-y (- (/ (.-height the-canvas) 2) paddle-half-height)
        ;; If ball is moving toward the paddle, then aim the
        ;; paddle toward the ball, otherwise toward the middle of
        ;; the screen
        y-to-move-toward (if (< (:dx ball) 0)
                           (- (:y ball) paddle-half-height)
                           middle-of-screen-y)
        y-direction (if (< (:y paddle) y-to-move-toward) 1 -1)
        new-y (+ (:y paddle) (* dt y-direction (:speed paddle)))
        distance-to-move (Math/abs (- y-to-move-toward (:y paddle)))]

    ;; Don't move if the paddle is close its destination.
    ;; This will help to prevent jitter caused by the paddle
    ;; overshooting its intended destination.
    (when (> distance-to-move 1)
      (swap! state update-in [:player-one-paddle :y] (constantly new-y)))))

(defn update-paddle-two! [dt]
  (let [ball (:ball @state)
        paddle (:player-two-paddle @state)

        ;; We want to move the paddle in a up or down direction
        ;; depending on if it is above or below the paddle
        y-direction (if (< (:y paddle) (:y ball)) 1 -1)
        new-y (+ (:y paddle) (* dt y-direction (:speed paddle)))]
    (swap! state update-in [:player-two-paddle :y] (constantly new-y))))

(defn update-paddles! [dt]
  (update-paddle-one! dt)
  (update-paddle-two! dt))

(defn move-ball! [dt]
  (let [ball (:ball @state)
        {:keys [x y dx dy speed]} ball

        x-movement (* dt dx speed)
        new-x (+ x-movement x)

        y-movement (* dt dy speed)
        new-y (+ y-movement y)

        updated-ball (assoc ball
                            :x new-x
                            :y new-y)]

    (swap! state assoc :ball updated-ball)))

(defn draw-ui! [ctx canvas-width canvas-height]
  (let [x (/ canvas-width 2)
        player-one-score (:player-one-score @state)
        player-two-score (:player-two-score @state)]
    (set! (.-fillStyle ctx) "#000000")
    (set! (.-font ctx) "30px Arial")
    (set! (.-textAlign ctx) "center")
    (.fillText ctx (str player-one-score "      " player-two-score) x 35)

    (doto ctx
      (.beginPath)
      (.setLineDash [5 15])
      (.moveTo x 5)
      (.lineTo x canvas-height)
      (.stroke))))

(defn draw-paddle!
  [{:keys [x y width height]} ctx]
  (.fillRect ctx x y width height))

(defn draw-paddles! [ctx]
  (draw-paddle! (:player-one-paddle @state) ctx)
  (draw-paddle! (:player-two-paddle @state) ctx))

(defn draw-ball! [{:keys [x y radius color]} ctx]
  (set! (.-fillStyle ctx) color)
  (doto ctx
    (.beginPath)
    (.arc x y radius 0 (* 2 Math/PI) true)
    (.closePath)
    (.fill)))

(defn draw-scene []
  (let [canvas the-canvas
        ctx (.getContext canvas "2d")]
    (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
    (draw-ui! ctx (.-width canvas) (.-height canvas))
    (draw-paddles! ctx)
    (draw-ball! (:ball @state) ctx)))

(defn request-animation-frame! [render-fn]
  (let [id (.requestAnimationFrame js/window render-fn)]
    (swap! state assoc :animation-frame-id id)))

(defn update! []
  (when (true? (:playing? @state))
    (let [canvas the-canvas
          now (.getTime (js/Date.))
          dt-ms (- now (:last-frame-time @state)) ;; milliseconds since last call
          dt (/ dt-ms 1000)] ;; seconds since last call

      (update-paddles! dt)
      (move-ball! dt)

    ;; Note that with a large delta time it's possible the
    ;; ball could travel through the paddle and we'd miss
    ;; a collision.
    ;; We could solve this by using a ray to check for
    ;; collisions, but we're not implementing that here.
      (handle-collisions! (.-height canvas))

    ;; If a player has scored, then increment the score
    ;; and serve the ball
      (cond
        (player-one-scored? (.-width canvas))
        (do
          (swap! state update-in [:player-one-score] inc)
          (swap! state update-in [:ball] #(mk-ball :player-two)))

        (player-two-scored?)
        (do
          (swap! state update-in [:player-two-score] inc)
          (swap! state update-in [:ball] #(mk-ball :player-one))))

      (swap! state assoc :last-frame-time now)
      (draw-scene)))
  (request-animation-frame! update!))

;; If the user changes tabs we want to pause the game
(defn handle-document-hidden! []
  (when-let [frame-id (:animation-frame-id @state)]
    (.cancelAnimationFrame js/window frame-id)))

(defn handle-document-visible! []
  ;; Reset the time so we don't get a potentially
  ;; large delta time on the next update
  (swap! state assoc :last-frame-time (.getTime (js/Date.)))
  (request-animation-frame! update!))

(defn handle-visibility-change! []
  (if js/document.hidden
    (handle-document-hidden!)
    (handle-document-visible!)))

(defn handle-start-button-click [_event]
  ;; Reset the time so we don't get a potentially
  ;; large delta time on the first frame
  (swap! state assoc :last-frame-time (.getTime (js/Date.)))
  (swap! state assoc :playing? true))

;; start is called by init and after code reloading finishes
(defn ^:dev/after-load start []
  (js/console.log "start")

  (js/document.addEventListener "visibilitychange" handle-visibility-change!)

  (events/listen
   (dom/getElement "button")
   (.-CLICK events/EventType)
   handle-start-button-click)

  (reset! state (mk-state))

  (update!))

(defn init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (js/console.log "init")
  (start))

;; this is called before any code is reloaded by shadow-cljs
(defn ^:dev/before-load stop []
  (js/console.log "stop")

  (js/document.removeEventListener "visibilitychange" handle-visibility-change!)

  (when-let [frame-id (:animation-frame-id @state)]
    (.cancelAnimationFrame js/window frame-id))

  (events/unlisten
   (dom/getElement "button")
   (.-CLICK events/EventType)
   handle-start-button-click))
