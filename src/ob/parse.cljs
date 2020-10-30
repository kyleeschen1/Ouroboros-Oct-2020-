(ns ob.parse
  (:require
   [clojure.walk :as w]
   [clojure.string :as str]
   [cljs.tools.reader :refer [read-string]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global State


(defonce global-env (atom {}))


(defn default-params []
  {:style {:style {:font-family "Courier"
                   :font-size "18px"
                   :font-weight "900"
                   :white-space "pre-wrap"
                   :list-style-type "none"
                   :opacity "1"
                   :position "relative"}}
   :indent 0
   :newline 0
   :replacement identity
   :pause 0
   :eval-keep-quoted []
   :eval-mode "complete"})

(defn get-function-copy [function-name]
  (atom (function-name @global-env)))

;;================================================================================
;; Add caller function

(defn add-caller [funcname args]
  (read-string (str "(" funcname " " (str/join " " args) ")")))

(defn add-caller-to-dm [funcname dm]
  (let [caller-symbol (add-caller funcname (:args @dm))]
    (swap! dm assoc :caller caller-symbol)))

(defn add-caller-to-params-map [pm]
  (swap! pm assoc :caller (default-params)))

;;================================================================================
;; Construct Tree Representations

(defn construct-tree-representations [name expression]
  
  "Builds an atomic map with a variety of tree representations of a function."
  
  (let [display-map (atom {})
        cleaned-map (atom {})
        params-map  (atom {})

        update-map (fn [mp kw value] (swap! mp assoc kw value))
        tagged? (fn [node] (and (coll? node) (= (first node) 'tag)))
        
        get-kw (fn [node] (second node))
        get-expr (fn [node] (last node))
        get-params (fn [node] (if (= 3 (count node))
                                (default-params)
                                (merge (default-params) (second (rest node)))))
        
        map-builder (fn [mp update-op return-op]
                      (fn [node]
                        (if (tagged? node)
                          (do (update-map mp (get-kw node) (update-op node))
                              (return-op node))
                          node)))

        construct-display-map (map-builder display-map get-expr get-kw)
        construct-cleaned-map (map-builder cleaned-map get-expr get-expr)
        construct-params-map  (map-builder params-map get-params get-expr) 
        
        just-doing-it-for-the-side-effects (w/postwalk construct-display-map expression)
        more-doing-it-for-the-side-effects (w/postwalk construct-params-map expression)
        original-expression  (w/postwalk construct-cleaned-map expression)

        ;; Add caller
        more-side-effects (add-caller-to-dm name display-map)
        even-more-side-effects (add-caller-to-params-map params-map)]
    
    (swap! global-env assoc name 
           {:name name
            :original-expression original-expression
            :cleaned-map @cleaned-map
            :display-map @display-map
            :params-map @params-map})))
