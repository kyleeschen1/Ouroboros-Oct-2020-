
(ns ob.environment
  (:require
   [ob.parse :refer [get-function-copy]]
   [clojure.walk :as w]
   [clojure.string :as str]
   [cljs.tools.reader :refer [read-string]]
   [cljs.js :refer [empty-state eval js-eval]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyword Appending and Indexing

(defn append-kws [kw counter]
  (keyword (str (name kw) "-" counter)))

(defn trim-kws [kw]
  (str/replace kw  #"-\d+" ""))

(defn append-kws-over-map [mp counter]
  (let [mod-kws (fn [[k v]]
                  {(append-kws k counter) v})]
    (into {} (map mod-kws mp))))

(defn build-attr-map [env]
  (let [display (:display-map @env)
        add-display (fn [[k v]] {k (merge v {:display (k display)})})]
    (->> @env
         (:params-map)
         (map add-display)
         (into {}))))

;; Within symbols
(defn add-index-to-kws-in-sym [symbol counter]
  (let [convert (fn [x] (if (keyword? x) (append-kws x counter) x))]
    (if (coll? symbol)
      (map convert symbol)
      (convert symbol))))

(defn add-index-to-all-displays [mp counter]
  (reduce (fn [mp kw]
            (update-in mp [kw :display] add-index-to-kws-in-sym counter))
          mp
          (keys mp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build New Environments

(declare persist-env)

(defn new-env

  ([fname] (new-env fname 0))
  
  ([fname level]
   (let [stack-pos (atom level)
         attrs (-> fname
                   (get-function-copy)
                   (build-attr-map)
                   (append-kws-over-map @stack-pos)
                   (add-index-to-all-displays  @stack-pos)
                   (atom))]
     (persist-env attrs stack-pos []))))

(defn persist-env [attrs stack-pos versions]
  
  (let [max-counter (atom @stack-pos)
        self (fn []
               (persist-env attrs
                            stack-pos
                            (into versions [{:attrs @attrs
                                             :stack-pos @stack-pos
                                             :max-counter @max-counter}])))]
    
    {
     'getter (fn getter
               ([kw type] (getter kw type @stack-pos))
               ([kw type pos]
                (-> @attrs
                    ((append-kws kw pos))
                    (type))))
     

     'setter (fn setter
               ([kw value type] (setter kw value type @stack-pos))
               ([kw value type pos]
                (let [k (append-kws kw pos)]
                  (swap! attrs assoc-in [k type] value)
                  (self))))

     'add-to-stack (fn [kw funcname]

                     ;; Reassign the keyword's value to the body
                     ;; of the expended function.
                     (swap! attrs assoc-in
                            [(append-kws kw @stack-pos) :display]
                            (append-kws :body (inc @stack-pos)))

                     ;; Update stack number, then create a new
                     ;; environment for expanded function. Add these
                     ;; new key-value pairs to attrs.
                     (swap! stack-pos inc)
                     (let [nenv (new-env funcname @stack-pos)]
                       (swap! attrs merge (('get-attrs nenv))))
                     (self))

     'get-attrs (fn [] @attrs)

     'get-versions (fn [] versions)

     'revert-to-prior-version (fn []
                                (let [vers (last versions)]
                                  (reset! attrs (:attrs vers))
                                  (reset! stack-pos (:stack-pos vers))
                                  (reset! max-counter (:max-counter vers))
                                  (persist-env attrs stack-pos (butlast versions))))


     ;;================================================================================
     ;; Stack manipulation

     'get-stack-pos (fn [] @stack-pos)

     'set-stack-pos (fn [pos]
                      (reset! max-counter @stack-pos)
                      (reset! stack-pos pos)
                      (self))

     'return-to-top (fn []
                      (reset! stack-pos @max-counter)
                      (self))

     'pop-stack (fn [decrease]
                  (swap! stack-pos decrease)
                  (reset! max-counter @stack-pos)
                  (self))

     }))

;; Access displays
(defn get-display [env kw]
  (('getter env) kw :display))

(defn set-display [env kw value]
  (('setter env) kw value :display))

(defn get-display-map [env]
  (let [attrs (('get-attrs env))
        remove-display-key (fn [[k v]] [k (:display v)])]
    (into {} (map remove-display-key attrs))))

(defn get-versions [env]
  (('get-versions env)))

(defn revert-to-prior-version [env]
  (('revert-to-prior-version env)))

;; Access other attributes
(defn get-attr [env kw attr]
  (('getter env) kw attr))

(defn set-attr [env kw attr value]
  (('setter env) kw value attr))

(defn get-attrs [env]
  (('get-attrs env)))

;; Access stack level
(defn return-to-root [env]
  (('set-stack-pos env) 0))

(defn return-to-top-of-stack [env]
  (('return-to-top env)))

(defn pop-stack
  ([env]
   (pop-stack env 1))
  
  ([env n]
   (('pop-stack env) #(- % n))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pipeline

;; Style
(defn set-indent [env kw]
  (set-attr env kw :style {:style {:position "relative"
                                   :left-margin "30px"}}))

(defn update-indents [style indents]
  (let [ shift (str (* indents 0) "px")]
    (-> style
        (assoc-in [:style :position] "relative")
        (assoc-in [:style :left] shift))))

(defn set-style [env kw attr value]
  (set-attr env kw :style (assoc-in (get-attr env kw :style) [:style attr] value)))

(defn set-newline [env kw]
  (set-attr env kw :newline 1))
