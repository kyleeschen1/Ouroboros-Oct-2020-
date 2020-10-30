(ns ^:figwheel-hooks ob.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  
  (:require
   
   [ob.parse :refer [get-function-copy
                     construct-tree-representations
                     global-env]]
   [ob.html-generator :refer [generate-html]]
   [ob.evaluation :as eval]
   [ob.environment :as env]
   
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   
   [clojure.string :as str]
   
   [cljs.tools.reader :refer [read-string]]
   [cljs.js :refer [empty-state eval js-eval]]
   [cljs.core.async :refer [<! timeout]]))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animations

(defn cycle-n-times [env n f]
  ((apply comp (repeat n f)) env))

(defn get-n-copies [n commands]
  (vec (mapcat identity (repeat n commands))))

                                       
(defn execute-with-pauses [env transform pause]
  (fn [] (go (doseq [t @transform]
               (<! (timeout @pause))
               (swap! env t)
               (js/console.log (str (env/get-display-map @env)))))))

(defn transforms->pipeline [& args]
  (vec (reduce concat (vec args))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demonstration

;;==============================================================================
;; Factorial

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))


(construct-tree-representations
 'factorial
 '(tag :def (defn factorial
              (tag :args [(tag :n n)])
              (tag :body 
                   (if (tag :pred (zero? (tag :n n)))
                     (tag :pos {:indent 2 :newline 1} 1)
                     (tag :neg {:indent 2 :newline 1}
                          (* (tag :n n)
                             (tag :base
                                  (factorial
                                   (tag :dec (dec (tag :n n))))))))))))


(defn factorial-transforms [env]
  (let [n (env/get-display env :n)
        
        initial        [#(eval/eval-expand % :caller)]
        
        when-expanding [
                        #(env/set-style % :pred :color "purple")
                        #(eval/eval-reg % :pred)
                        #(env/set-style % :pred :color nil)
                        
                        #(env/set-style % :neg :color "purple")
                        #(eval/eval-quote % :body :neg)
                        #(env/set-style % :neg :color nil)

                        
                        #(env/set-style % :dec :color "purple")
                        #(eval/eval-reg % :dec)
                        #(env/set-style % :dec :color nil)


                        
                        #(env/set-style % :base :color "purple")
                        #(eval/eval-expand % :base)
                        #(env/set-style % :body :color "black")
                        ]

        base-case      [
                        #(env/set-style % :caller :color nil)
                        #(eval/eval-reg % :pred)
                        #(env/set-style % :pred :color "black")
                        #(env/set-style % :pos :color "purple")
                        #(eval/eval-pop  % :body)
                        ]
        
        when-contracting [
                          #(env/set-style % :base :color "purple")
                          #(env/set-style % :neg :color "purple")
                          #(eval/eval-pop % :body)
                          ]]
    
    (transforms->pipeline initial
                          (get-n-copies n when-expanding)
                          base-case
                          (get-n-copies n when-contracting))))


;;==============================================================================
;; Tail Recursive


(construct-tree-representations
 'factorial-acc
 '(tag :def (defn factorial-acc
              (tag :args [(tag :n n) (tag :acc acc)])
              (tag :body 
                   (if (tag :pred (zero? (tag :n n)))
                     (tag :pos {:indent 2 :newline 1} (tag :acc acc))
                     (tag :neg {:indent 2 :newline 1}
                          (tag :base
                               (factorial-acc
                                (tag :dec (dec (tag :n n)))
                                (tag :mult (* (tag :n n) (tag :acc acc)))))))))))



(defn factorial-acc-transforms [env]
  (let [n (env/get-display env :n)
        
        initial [#(eval/eval-expand % :caller)]
        
        when-expanding [
                        #(env/set-style % :pred :color "purple")
                        #(eval/eval-reg % :pred)
                        #(env/set-style % :pred :color nil)
                        
                        #(env/set-style % :neg :color "purple")
                        #(eval/eval-quote % :body :neg)
                        #(env/set-style % :neg :color nil)

                                                
                        #(env/set-style % :dec :color "purple")
                        #(eval/eval-reg % :dec)
                        #(env/set-style % :dec :color nil)

                                                                        
                        #(env/set-style % :mult :color "purple")
                        #(eval/eval-reg % :mult)
                        #(env/set-style % :mult :color nil)
                        
                        #(env/set-style % :base :color "purple")
                        #(eval/eval-expand % :base)
                        #(env/set-style % :body :color "black")
                        ]

        base-case      [
                        #(env/set-style % :pred :color "purple")
                        #(eval/eval-reg % :pred)
                        #(env/set-style % :pred :color "black")
                        #(env/set-style % :pos :color "purple")
                        #(eval/eval-reg % :body)
                        ]
        ]
    
    (transforms->pipeline initial
                          (get-n-copies n when-expanding)
                          base-case)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Continuation Passing Style

(fn fac-cps [n k]
  (if (zero? n)
    (k 1)
    (fac-cps (dec n) (fn [v] (k (* v n))))))

(construct-tree-representations
 'factorial-cps
 '(tag :def (defn factorial-cps
              (tag :args [(tag :n n) (tag :k k)])
              (tag :body 
                   (if (tag :pred (zero? (tag :n n)))
                     (tag :pos {:indent 2 :newline 1} ((tag :k k) 1))
                     (tag :neg {:indent 2 :newline 1}
                          (tag :base
                               (factorial-cps
                                (tag :dec (dec (tag :n n)))
                                (tag :cont
                                     (fn (tag :cont-args [(tag :v v)])
                                       (tag :cont-body
                                            ((tag :k k)
                                             (tag :mult (* (tag :v v)
                                                           (tag :n n))))))))))
                     )))))




(defn factorial-cps-transforms [env]
  
  (let [n (env/get-display env :n)
        
        initial [#(env/set-display % :k  'identity)
                 #(eval/eval-expand % :caller)]
        
        
        when-expanding [
                     ;;   #(set-style % :pred :color "purple")
                        #(eval/eval-reg % :pred)
                     ;;   #(set-style % :pred :color nil)
                        
                    ;;    #(set-style % :neg :color "purple")
                        #(eval/eval-sub % :body :neg)
                    ;;    #(set-style % :neg :color nil)
               
                   ;;     #(set-style % :dec :color "purple")
                        #(eval/eval-reg % :dec)
                   ;;     #(set-style % :dec :color nil)
                       
                   ;;     #(set-style % :base :color "purple")
                        #(eval/eval-expand % :base)
                   ;;     #(set-style % :base :color nil)
                        ]

        base-case      [
                   ;;     #(set-style % :pred :color "purple")
                        #(eval/eval-reg % :pred)
                   ;;     #(set-style % :pred :color "black")
                      ;;  #(eval-sub % :body :pos)
                        #(eval/eval-fn-reduce % :cont :pos 1)
                       ;;  #(eval-sub % :body :cont-body)
                  ;;      #(set-style % :pos :color "green")
                        ]
        when-contracting [#(eval/eval-fn-reduce % :cont :mult 1)
                          ]
        ]
    
    (transforms->pipeline initial
                          (get-n-copies n when-expanding)
                          base-case
                          (get-n-copies n when-contracting))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML Widgets

(declare current)
(declare transforms)
(declare transform)
(declare counter)


(def fs "18px")

(defn insert-n-breaks [n]
  [:div (take n (repeat [:br]))])

(defn set-var-button [env kw]
  
  (let [trimmed-kw (env/trim-kws (str (name kw)))
        button-name (keyword (str "input." trimmed-kw))]
    
    [:div
     
     [:label {:style {:font-size fs
                      :font-family "Courier"
                      :font-weight "900"}}
      (str "Set " trimmed-kw  " ")]

     
     [:br]
     
     [:input {:type "text"
              :style {:border "3px solid black" :font-size fs  :font-family "Courier"}
              :on-change (fn [value]
                           (let [val (-> value  .-target .-value)]
                             (swap! env #(env/set-display % trimmed-kw val))
                             (reset! transform ((@current @transforms) @env))))}]
     [:br]]))

(defn create-input-fields-for-vars [env]
  (let [args (env/get-display @env :args)]
    [:div (doall (map #(set-var-button env %1) (vec args)))]))

(defn create-radio-button [funcname label env current transforms counter]
  [:div
   
    [:input {:type "radio"
            :style {:width "12px" :height "12px"}
            :name "function"
            :checked (= @current funcname)
            :on-change (fn [e]
                         (reset! env (env/new-env funcname))
                         (reset! current funcname)
                         (reset! counter 0))
            }]
   
   [:label {:for "factorial-rec"
            :style {:font-size fs
                    :font-weight "900"
                    :font-family "Courier"}}
    label]])



(defn computation-rate-toggle [env pause]
   [:input {:type "range"
                 :min 1
                 :max 10
                 :value 5
                 :on-change (fn [e]
                              (let [speed (js/parseInt ( ..  e -target -value))]
                                (reset! pause (/ speed 100))))}
    ])

(defn create-compute-button [env transform pause]
  [:button
   {:style {:font-family "Courier"
            :border "3px solid black"
            :cursor "pointer"
            :font-size "15px"
            :font-weight "900"
            :background-image "url(./images/background.png)"}
    :on-click (execute-with-pauses env transform pause)}
   "Compute All"])

(defn execute-incrementally [env transform counter]
  (fn [e]
    (let [tr (get @transform @counter)]
      (swap! env tr)
      (swap! counter inc))))

(defn create-next-button [env transform counter]
  [:button
   {:style {:font-family "Courier"
            :border "3px solid black"
            :cursor "pointer"
            :font-size "15px"
            :font-weight "900"
            :background-image "url(./images/background.png)"}
    :on-click (execute-incrementally env transform counter)}
   "Next Step"])

(defn create-prev-button [env]
   [:button
   {:style {:font-family "Courier"
            :border "3px solid black"
            :cursor "pointer"
            :font-size "15px"
            :font-weight "900"
            :background-image "url(./images/background.png)"}
    :on-click (fn []
                (do
                  (swap! env env/revert-to-prior-version)
                  (swap! counter dec)))}
   "Revert"])

(defn create-control-dashboard [env  counter]
  
  (let [pause (atom 600)]
    
    [:div {:style {:width "20%"
                   :float "left"
                   :padding "5px"
                   :border "4px solid black"}}
     
     [:h1 {:style {:text-align "center"
                   :font-family "Courier"}}  "Factorial" ]
     
     (insert-n-breaks 2)
     (create-radio-button 'factorial "Recursive" env current transforms counter)
     
     (insert-n-breaks 1)
     (create-radio-button 'factorial-acc "Tail-Recursive" env current transforms counter)
     
     (insert-n-breaks 1)
     (create-radio-button 'factorial-cps "Continuation Passing" env current transforms counter)
     
     (insert-n-breaks 4)
     (create-input-fields-for-vars env)
     
     (insert-n-breaks 3)
     (create-next-button env transform counter)
     
     (insert-n-breaks 1)
     (create-compute-button env transform pause)
     
     (insert-n-breaks 3)

     [:h3 "Function Call"]
    [:span {:style {:font-size fs :font-family "Courier" }}
     (str (get @transform @counter))]

     [:h3 "Display Map"]
         [:span {:style {:font-size fs :font-family "Courier"}}
          (str (env/get-display-map @env))]
     
     ]))


(defn create-code-dashboard [env]
  [:div  {:style {:width "75%"
                  :margin-top "10px"
                  :float "left"
                  :padding "15px"}}
   (generate-html @env :caller-0)])

;;==============================================================================
;; HTML

(defonce transforms (atom {'factorial factorial-transforms
                           'factorial-acc factorial-acc-transforms
                           'factorial-cps factorial-cps-transforms}))

(defonce current (atom 'factorial))
(defonce env (atom (env/new-env @current)))
(defonce transform (atom nil))
(defonce counter (atom 0))

;;(js/alert @transform)

(defn html []
  [:div {:style {:padding-left "10px" :padding-top "40px"}}
   (create-control-dashboard env counter)
   (create-code-dashboard env) ])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering

(defn get-app-element []
  (gdom/getElement "app"))

(defn mount [el]
  (rdom/render [html] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda
;; 1) Change timer to allow piping
;; 2) Add reversion
;; 3) Lambda evaulation
;; 3) Add opacity styling
;; 4) Add font size and speed ranges
;; 

;; Let functions go offscreen
;; Make al functions return to the same place


;; 1) Import
;; 2) Style manipulation (opacity decay)
;; 3) Switch indents to tabs

;; Evaluation helpers
