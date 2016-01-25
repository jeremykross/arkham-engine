(ns arkham-engine.core
  (:require 
    [cljs.core.async :as async]
    [reagi.core :as reagi]
    [monet.canvas :as monet])
  (:require-macros
    [cljs.core.async.macros :as async-mac]))

(enable-console-print!)

(declare on-ground-signal)
(declare on-screen)
(declare tiles-on-screen)

(def canvas (.getElementById js/document "arkham"))
(def ctx (monet/get-context canvas "2d"))
(def screen-width 640)
(def screen-height 360)
(def frame-count (atom 0))
(def update-count (atom 0))

(defn load-image
  [path]
  (let [img (js/Image.)]
    (set! (.-src img) path)
    img))

(defn set-image-smoothing!
  [ctx enabled]
  (set! (.-imageSmoothingEnabled ctx) enabled))

(defn make-frames
  [s-x s-y f-w f-h per-row from to]
  (reduce (fn [frames f]
            (let [col (int (/ f per-row))
                  row (mod f per-row)]
              (conj frames 
                    [(* row f-w)
                     (* col f-h)
                     f-w f-h])))
          []
          (range from to)))

(defn array->tilemap
  [array tile-width tile-height per-row]
  (reduce (fn [tiles [index tile]])
         []
         (map-indexed (fn [index item] [index item]) array)))

(set-image-smoothing! ctx false)

(defonce app-state 
  {:screen {:virtual [0 0 (* 1 (/ screen-width screen-height)) 1]
            :actual [0 0 screen-width screen-height]}
   :spritesheets {:batman {:sheet (load-image 
                                    "images/hero.png")
                           :standing [[0 0 32 32]]}}

   :entities {:player {:pos [64 134]
                       :vel [0 0]
                       :acc [0 (/ 98 60)]
                       :dimensions [64 64]
                       :sheet :batman
                       :animation :standing
                       :frame 0}}
   :tilemap 
   (map (fn [tile-index]
          (let [[width height] [128 128]]
            {:pos [(* tile-index width)
                   (+ ;(* (js/Math.sin (/ tile-index 4)) 64)
                   (- screen-height 64))]
             :dimensions [width height]
             :tile :ice})) (range 100))
   :tiles {:sheet (load-image "images/tiles.png")
           :ice [0 0 16 16]}})

(set! (.-onerror js/window)
      (fn [mesg _ line col error] 
        (js/alert (str mesg " at " line ":" col "\n" (.-stack error)))))

(defn on-js-reload [])

(defn length
  [v]
  (js/Math.sqrt
    (apply + (map * v v))))

(defn normalize
  [v]
  (map #(/ % (length v)) v))

(defn bounding-rect
  [e]
  (let [[x y] (:pos e)
        [w h] (:dimensions e)
        left (- x (/ w 2))
        top (- y (/ h 2))
        right  (+ left w)
        bottom (+ top h)]
    [left top right bottom]))

(defn collides-with
  [a b]
  (let [[left-a top-a right-a  bottom-a] (bounding-rect a)
        [left-b top-b right-b  bottom-b] (bounding-rect b)]
    (if (and 
          (< left-a right-b)
          (> right-a left-b)
          (< top-a bottom-b)
          (> bottom-a top-b))
      (let [[from-x from-y] (map - (:pos b) (:pos a))
            [w-a h-a] (:dimensions a)
            [w-b h-b] (:dimensions b)
            overlap-x (- (+ (/ w-a 2) (/ w-b 2)) 
                         (js/Math.abs from-x))
            overlap-y (- (+ (/ h-a 2) (/ h-b 2)) 
                         (js/Math.abs from-y))]
            {:collides-with b
             :overlap [overlap-x overlap-y]}))))

(defn on-screen
  [e world-offset screen-dimensions]
  (collides-with e {:pos (map + world-offset
                              (map #(/ % 2) 
                                   screen-dimensions))
                    :dimensions screen-dimensions}))


(defn collisions
  [to-check against]
  (filter #(not (empty? %))
          (map (partial collides-with to-check)
               against)))

(defn depenetrate
  [entity collisions collided-from]
  (let [max-overlap (reduce (fn [max-overlap c]
                              (map max max-overlap 
                                   (map *
                                        collided-from
                                        (:overlap c))))
                            [0 0] collisions)]
    (assoc entity :pos
           (map - (:pos entity) (map #(* 1.1 %) max-overlap)))))


(defn update-velocity
  [entity collisions collided-from]
  (let [collided-from (if (pos? (count collisions)) 
                        (map #(bit-xor % 1) collided-from)
                        [1 1])
        new-v (map * 
                   (:vel entity)
                   collided-from)]
    (assoc entity :vel new-v)))

(defn sample-on
  [sample-sig on-sig]
  (reagi/map #(deref sample-sig) on-sig))

(defn spritesheet-for
  [entity]
  (get-in app-state
          [:spritesheets (:sheet entity)]))


(def time-sig (reagi/events (js/Date.now)))
(def second-sig (reagi/sample 1000 reagi/time))

(def fps-sig (reagi/map 
               (fn [] 
                 (let [fc @frame-count
                       uc @update-count]
                   (reset! frame-count 0)
                   (reset! update-count 0)
                   [uc fc])) second-sig))

(comment dt-sig (reagi/map (fn [[last-t t]] 
                         (- t last-t))
                       (reagi/buffer 2 time-sig)))

(comment mouse-position-sig (reagi/events [0 0]))
(comment mouse-down-sig (reagi/events false))
(def key-down-sig (reagi/uniq (reagi/events {:key -1 :key-state :down})))
(def key-up-sig (reagi/uniq (reagi/events {:key -1 :key-state :up})))

(defn key-down?
  [key-code]
  (reagi/map 
    (fn [k]
      (.log js/console (str k))
      (= (:key-state k) :down))
    (reagi/filter #(= (:key %) key-code)
                  (reagi/merge key-down-sig 
                               key-up-sig))))

(def right-signal (key-down? 40))

(def arrows
  (reagi/map 
    (fn [[w a s d]]
      {:w w
       :a a
       :s s
       :d d})
    (reagi/zip (key-down? 38)
               (key-down? 37)
               (key-down? 40)
               (key-down? 39))))

(comment mouse-down-position-sig
  (reagi/map
    (fn [mouse-down]
      @mouse-position-sig)
    mouse-down-sig))

(comment mouse-delta-sig
  (reagi/map
    (fn [[mouse-prev mouse-pos]]
      (map -  mouse-pos mouse-prev))
    mouse-down-position-sig))

(comment mouse-down-duration-sig
  (reagi/map
    (fn [[t [mouse-is-down timestamp]]]
      (if mouse-is-down 
        (- t timestamp)
        0))
    (reagi/zip 
      time-sig
      (reagi/map 
        (fn [x] [x @time-sig])
        mouse-down-sig))))

(def punch-sig
  (let [input-chan
          (reagi/subscribe (reagi/uniq right-signal)
                           (async/chan (async/dropping-buffer 1)))
          punch-signal (reagi/events false)]
      (async-mac/go
        (loop []
          (when-let [foo (async/<! input-chan)]
            (reagi/deliver punch-signal true)
            (async/<! (async/timeout 200))
            (reagi/deliver punch-signal false))
          (recur)))
      punch-signal))

(defn jump
  [e jumping]
  (let [[vx vy] (:vel e)
        can-jump (and jumping (:on-ground e) (not (:punching e)))
        jump-vel (if can-jump -19 0)]
    (assoc e :vel [vx (+ vy jump-vel)])))

(defn punch
  [e punching]
  (let [[vx vy] (:vel e)
        can-punch (and punching (not (:punching e)))
        punch-vel (if can-punch 200 0)]
    (-> e
      (assoc :punching punching)
      (assoc 
        :vel [(+ vx punch-vel) vy]))))

(defn handle-input
  [entity]
  (let [wasd @arrows
        punching @punch-sig
        [vx vy] (:vel entity)
        [ax ay] (:acc entity)
        v-change 10]
    (-> entity
      (assoc 
        :vel [(cond 
                (:d wasd) (if (:on-ground entity) v-change v-change)
                (:a wasd) (if (:on-ground entity) (- v-change) (- v-change))
                :else 0) 
              vy])
      (punch punching)
      (jump (:w wasd)))))


(defn physics
  [entity]
  (let [new-v (mapv + (:vel entity) (:acc entity))
        new-pos (mapv + (:pos entity) (:vel entity))
        pos-change (map - new-pos (:pos entity))]
    (assoc entity
           :dir (if (= new-pos (:pos entity)) 
                  [0 0]
                  (normalize pos-change))
           :prev-pos (:pos entity)
           :pos new-pos
           :vel new-v)))

(defn dampen
  [entity]
  (if (:on-ground entity)
    (assoc entity :vel (map #(* % 0.85) (:vel entity)))
    entity))

(defn floor 
  [entity]
  (let [[pos-x pos-y] (:pos entity)]
    (-> entity
      (assoc :pos [pos-x (min pos-y 200)])))) 
             
(defn bound
  [entity tilemap]
  (let [[x y] (:pos entity)
        [prev-x prev-y] (:prev-pos entity)
        collision-x (collisions (assoc entity 
                                       :pos [x prev-y]) tilemap)
        collision-y (collisions (assoc entity 
                                       :pos [prev-x y]) tilemap)]
    (-> entity
      (depenetrate collision-y [0 1])
      (depenetrate collision-x [1 0])
      (update-velocity collision-y [0 1])
      (update-velocity collision-x [1 0])
      (assoc :on-ground (pos? (count collision-y)))
      (dampen))))
         
(def entity-signal
  (reagi/reduce
    (fn [entity]
      (swap! update-count inc)
      (-> entity
        handle-input
        physics
        (bound @tiles-on-screen))) 
    (get-in app-state [:entities :player])
    time-sig))


(def on-ground-signal
  (reagi/map (fn [e] 
               (= (get-in e [:vel 1]) 0))
             entity-signal))


(def world-offset-signal
  (reagi/map (fn [[wx wy]] [(- wx (/ screen-width 2))
                            (- wy (/ screen-height 2))])

             (reagi/reduce (fn [world-offset entity]
                             (let [[wx wy] world-offset
                                   [ex ey] (:pos entity)]
                               (let [diff-x (- (+ ex 0) wx)
                                     diff-y (- (- ey 0) wy)]
                                 [(+ wx (/ diff-x 10)) 
                                  (+ wy (/ diff-y 10))])))
                           [0 0]
                           entity-signal)))

(def tiles-on-screen
  (reagi/map
    (fn [world-offset]
      (filter #(on-screen % 
                        world-offset
                        [screen-width 
                         screen-height])
            (:tilemap app-state)))
    world-offset-signal))

(comment deliver-touch 
  [evt]
  (let [touch (.item (.-touches evt) 0)]
    (reagi/deliver mouse-position-sig
                   [(.-clientX touch)
                    (.-clientY touch)])))

(comment deliver-mouse
  [evt]
  (reagi/deliver mouse-position-sig
                 [(.-clientX evt)
                  (.-clientY evt)]))

(comment
(.addEventListener js/document.body "touchmove" 
                   deliver-touch)

(.addEventListener js/document.body "mousemove" 
                   deliver-mouse)

(.addEventListener js/document.body "touchstart"
                   (fn [evt]
                     (deliver-touch evt)
                     (reagi/deliver mouse-down-sig true)))

(.addEventListener js/document.body "mousedown"
                   (fn [evt]
                     (deliver-mouse evt)
                     (reagi/deliver mouse-down-sig true)))

(.addEventListener js/document.body "touchend"
                   (fn [evt]
                     (reagi/deliver mouse-down-sig false)))

(.addEventListener js/document.body "mouseup"
                   (fn [evt]
                     (reagi/deliver 
                       mouse-down-sig false)))
)

(.addEventListener js/document.body "keydown"
                   (fn [evt]
                     (reagi/deliver key-down-sig 
                                    {:key-state :down
                                     :key (.-keyCode evt)})))

(.addEventListener js/document.body "keyup"
                   (fn [evt]
                     (.log js/console (str "up: " (.-keyCode evt)))
                     (reagi/deliver key-up-sig 
                                    {:key-state :up
                                     :key (.-keyCode evt)})))
(defn change-space
  [[x y] 
   [from-left from-top from-right from-bottom]
   [to-left to-top to-right to-bottom]]
  (let [x-pct (/ (- x from-left) 
                 (- from-right from-left))
        y-pct (/ (- y from-top)
                 (- from-bottom from-top))]
    [(* x-pct (+ to-left (+ to-left to-right)))
     (* y-pct (+ to-top (+ to-top to-bottom)))]))

(defn to-world
  [pos screen]
  (change-space pos (:actual screen) (:virtual screen)))

(defn to-screen
  [pos screen]
  (change-space pos (:virtual screen) (:actual screen)))

(defn draw-entity!
  [ctx e spritesheets]
  (let [[x y] (:pos e)
        [w h] (:dimensions e)
        sheet ((:sheet e) spritesheets)
        anim ((:animation e) sheet)
        [sx sy sw sh] (get anim (:frame e))]
    (monet/draw-image ctx
                      (:sheet sheet)
                      {:sx sx :sy sy :sw sw :sh sh
                       :dx (int (- x (/ w 2))) :dy (int (- y (/ h 2))) :dw w :dh h})))

(defn draw-entities!
  [ctx entities spritesheets]
  (doseq [e entities] (draw-entity! ctx e spritesheets)))


(defn draw-tilemap!
  [ctx tilemap tiles]
  (doseq [t tilemap]
    (let [[x y] (:pos t)
          [w h] (:dimensions t)
          [sx sy sw sh] ((:tile t) tiles)]
      (monet/draw-image ctx 
                        (:sheet tiles)
                        {:sx sx :sy sy :sw sw :sh sh
                         :dx (int (- x (/ w 2)))
                         :dy (int (- y (/ h 2)))
                         :dw w :dh h}))))


(defn render []
  (swap! frame-count inc)
  (monet/save ctx)
  (monet/fill-style ctx "cornflowerblue")
  (monet/fill-rect ctx {:x 0 :y 0 :w 640 :h 360})
  (monet/fill-style ctx "white")
  (let [[wx wy] @world-offset-signal
        tiles @tiles-on-screen]
    (monet/translate ctx (- (int wx)) (- (int wy)))
    (draw-entity! ctx @entity-signal
                  (:spritesheets app-state))
    (draw-tilemap! ctx 
                   tiles
                   (:tiles app-state)))
  (monet/restore ctx))

(monet.core/animation-frame
  (fn frame []
    (monet.core/animation-frame frame)
    (render)
    (reagi/deliver time-sig (js/Date.now))))
