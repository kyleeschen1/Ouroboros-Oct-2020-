(ns ob.html-generator
  (:require
   [ob.environment :as env]
   [clojure.string :as str]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Horizontal Anchoring

(defn hz-anchor [anchor current-line-count]
  (atom {:anchor anchor
         :current-line-count current-line-count}))

(defn add-to-current-line-count [h-anchor line]
  (swap! h-anchor update :current-line-count #(+ %  (count line))))

(defn reset-current-line-count

  ([h-anchor]
   (reset-current-line-count h-anchor 0))

  ([h-anchor val]
   (swap! h-anchor assoc :current-line-count val)))

(defn update-anchor [h-anchor]
  (swap! h-anchor assoc :anchor (+ (:anchor @h-anchor)
                                   (:current-line-count @h-anchor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating HTML

(declare is-env-var?)
(declare is-body?)
(declare create-html-tag)
(declare reduce-into-html)
(declare build-nested-html)
(declare create-str-of-n-copies)
(declare parse-symbol-into-strings)
(declare trim-parens-spaces)
(declare string-to-kw)


(defn generate-html

  ([env]
   (generate-html env :body-0 (hz-anchor 0 0)))

  ([env kw]
   (generate-html env kw (hz-anchor 0 0)))
  
  ([env kw h-anchor]
   
   (let [kw-attrs (kw (env/get-attrs env))
         display (parse-symbol-into-strings (:display kw-attrs))
         style (:style kw-attrs)

         newlines (:newline kw-attrs)
         indents (* 2 (:indent kw-attrs))
         indents (if (zero? newlines)
                   indents
                   (do
                     (reset-current-line-count h-anchor indents)
                     (+ indents (:anchor @h-anchor))))
         
         indents (create-str-of-n-copies indents "\u00A0")
         newlines (create-str-of-n-copies newlines "\n")
         opening (str/join "" (concat newlines indents))

        ;;  temp (add-to-current-line-count h-anchor indents)

         init-div [(create-html-tag kw) style opening]]

     
     (if (coll? display)
       (reduce-into-html display env init-div h-anchor)
       (reduce-into-html (vector display) env init-div h-anchor)))))


(defn reduce-into-html [display env init-div h-anchor]
  (->> display
       (reduce (build-nested-html env h-anchor)
               init-div)))


(defn build-nested-html [env h-anchor]
  (fn [x y]
    (cond
      (is-body? y env) (do
                         (update-anchor h-anchor)
                         (reset-current-line-count h-anchor)
                         (into x [(generate-html env (string-to-kw y) (hz-anchor (:anchor @h-anchor)
                                                                                 (:current-line-count @h-anchor)))]))
      
      (is-env-var? y env) (into x [(generate-html env (string-to-kw y) h-anchor)])
      
      :else (let [string (trim-parens-spaces (str y " "))]
              (do
                (add-to-current-line-count h-anchor string)
                (into x string))))))


;; HTML helpers

(def regex-opening-paren (re-pattern "\\)"))
(def regex-closing-paren (re-pattern "\\("))
(def regex-opening-paren-whitespace (re-pattern "\\(\\s"))
(def regex-closing-paren-whitespace (re-pattern "\\s\\)"))


(defn parse-symbol-into-strings [string]
  (-> string
      (str)
      (str/replace regex-opening-paren  " )")
      (str/replace regex-closing-paren "( ")
      (str/split #" ")))

(defn trim-parens-spaces [string]
  (-> string
      ;; (str/replace regex-opening-paren-whitespace "(")
      (str/replace regex-closing-paren-whitespace ")")))

(defn create-str-of-n-copies [n txt]
  (->> txt
       (repeat)
       (take n)
       (doall)
       (vec)
       (str/join "")))

(defn string-to-kw [string]
  (keyword (subs string 1)))

(defn kw-eq-in-list? [txt kws]
  (let [kw (keyword (subs txt 1))]
    (some #(= % kw) kws)))

(defn is-env-var? [txt env]
  (kw-eq-in-list? txt (keys (env/get-display-map env))))

(defn is-body? [txt env]
  (let [rgx (fn [kw] (re-matches #":body-." (str kw)))]
    (kw-eq-in-list? txt (->> env
                              (env/get-display-map)
                              (keys)
                              (filter rgx)))))

(defn create-html-tag [kw]
  (keyword (str "span." (name kw))))
