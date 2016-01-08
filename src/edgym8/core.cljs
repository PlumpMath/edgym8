(ns edgym8.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require
   [re-frame.core :refer [register-handler
                          debug
                          path
                          register-sub
                          dispatch
                          dispatch-sync
                          subscribe]]
   [reagent.core :as reagent :refer [atom]]
   ))

(enable-console-print!)

;;; Handlers
;;; ------------------------------------------

(register-handler
 :init-db
 (fn [_ _] {:edge-data nil :mode :ready}))

(defn img-data-convert [data]
  (let [i (new js/Image)]
    (.addEventListener i
      "load"
      (fn [_]
        (let [w (.-width i) h (.-height i)
              c (.createElement js/document "canvas")
              ctx (.getContext c "2d")]
          (set! (.-width c) w)
          (set! (.-height c) h)
          (.drawImage ctx i 0 0)
          (dispatch [:img-loaded {:w w :h h :pxls (.-data (.getImageData ctx 0 0 w h))}]))))
    (set! (.-src i) data)))

(defn rgba-to-grayscale [rgba-data]
  (vec (for [pxl-idx (range 0 (.-length rgba-data) 4)]
         (let [r (aget rgba-data pxl-idx)
               g (aget rgba-data (+ pxl-idx 1))
               b (aget rgba-data (+ pxl-idx 2))]
           (.floor js/Math
                   (+ (* r 0.2126)
                      (* g  0.7152)
                      (* 0.0722)))))))

(def v-filter
  [-1 0 1
   -2 0 2
   -1 0 1])

(def h-filter
  [1  2  1
   0  0  0
   -1 -2 -1])

(register-handler
 :img-loaded
 (fn [db [_ img-data]]
   (let [w (:w img-data) h (:h img-data)
         mc-vals (rgba-to-grayscale (:pxls img-data))
         edge-vals (for [y (range h)
                         x (range w)]
                    (let [idx (+ x (* y w))
                          kernel (map #(get mc-vals % 0)
                                      [(- idx w 1)     (- idx w) (+ (- idx w) 1)
                                       (- idx 1)       idx       (+ idx 1)
                                       (- (+ idx w) 1) (+ idx w) (+ idx w 1)])
                          v-edge (reduce + (map * kernel v-filter))
                          h-edge (reduce + (map * kernel v-filter))]
                      (min (.floor js/Math (.sqrt js/Math (+ (* v-edge v-edge) (* h-edge h-edge)))) 255)
                      ))]
     (assoc db
            :edge-data {:edge-vals edge-vals :w w :h h}))))

(register-handler
 :load-image
 (fn [db [_ data]]
   (img-data-convert data)
   db))

(register-handler
 :drop
 (fn [db [_ f]]
   (let [fr (new js/FileReader)]
     (set! (.-onload fr) (fn [e] (dispatch [:load-image (.. e -target -result)])))
     (.readAsDataURL fr f))
   (assoc db :edge-data nil :mode :loading)))

(register-handler
 :done
 (fn [db _]
   (assoc db :mode :ready)))

;;; Subscribers
;;; ------------------------------------------

(register-sub :edge-data
              (fn [db _]
                (reaction (:edge-data @db))))

(register-sub :mode
              (fn [db _]
                (reaction (:mode @db))))

;;; Components
;;; ------------------------------------------

(defn update-canvas [this]
  (when (-> this reagent/props :edge-vals)
    (let [edge-data (-> this reagent/props)
          c (.getElementById js/document "edgecanvas")
          ctx (.getContext c "2d")]
      (doseq [[idx v] (map-indexed vector (:edge-vals edge-data))]
        (let [x (mod idx (:w edge-data))
              y (/ idx (:w edge-data))
              col (str "rgba(" v "," v "," v ",255)")]
          (set! (.-fillStyle ctx) col)
          (.fillRect ctx x y 1 1)
          ))
      (dispatch [:done]))))

(defn edge-canvas [edge-data]
  (reagent/create-class
   {:reagent-render
    (fn [edge-data]
      [:canvas#edgecanvas {:width (:w edge-data) :height (:h edge-data)}])
    :component-did-mount update-canvas
    :component-did-update update-canvas}))

(defn scaffold []
  (let [state (atom {:hovered false})
        set-hovered #(swap! state assoc :hovered %)
        mode (subscribe [:mode])
        edge-data (subscribe [:edge-data])]
    (fn []
      (let [hovered (:hovered @state)]
        [:div
         [:h1 "Amazing Edge Finder"]
         [:div {:style {:width       "500px"
                        :height      "500px"
                        :line-height "500px"
                        :float       "left"
                        :border      (if (= @mode :ready) "1px solid #22aaee" "1px solid #eeaa22")
                        :color       (if (= @mode :ready) "#22aaee" "#eeaa22")
                        :text-align  "center"
                        :background (if (and (= @mode :ready) hovered) "#eeffff" "transparent")}
                :on-drop (fn [ev]
                           (.preventDefault ev)
                           (.stopPropagation ev)
                           (set-hovered false)
                           (when (= @mode :ready)
                             (dispatch [:drop (aget (.. ev -dataTransfer -files) 0)])))
                :on-drag-enter #(do (.preventDefault %)
                                    (set-hovered true))
                :on-drag-leave #(do (.preventDefault %)
                                    (set-hovered false))
                :on-drag-over (fn [ev] (.stopPropagation ev) (.preventDefault ev))}
          [:div {:style {:display "inline-block"
                         :line-height "16px"
                         :vertical-align "middle"}}
           (if (= @mode :ready) "DROP IMG HERE" "LOADING")]]
         (when @edge-data
           [edge-canvas @edge-data])]))))

;;; Plumbing
;;; ------------------------------------------

(defn application []
  (fn render-root [] [:div [scaffold]]))

(defn mount-root []
  (dispatch-sync [:init-db])
  (reagent/render [#'application]
                  (js/document.getElementById "app")))

(defn ^:export run [] (mount-root))

(defn on-js-reload [] (mount-root))
