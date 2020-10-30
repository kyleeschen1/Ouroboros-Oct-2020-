(ns ob.evaluation
  (:require
   [ob.environment :as env]
   [clojure.walk :as w]
   [clojure.string :as str]
   [cljs.tools.reader :refer [read-string]]
   [cljs.js :refer [empty-state eval js-eval]]))

(declare refresh-symbol-vals)
(declare refresh-symbol-vals-quote)
(declare cycle-through-tree)
(declare most-recent-kw)
(declare bind-args)
(declare eval-str)
(declare eval-sub)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation Functions

(defn eval-reg [env kw]
  (-> env
      (refresh-symbol-vals kw)
      (str)
      (eval-str)
      (#(env/set-display env kw %))))

(defn eval-expand [env kw]
  (let [expr (env/get-display env kw)
        kws (env/get-display env :args)
        funcname (first expr)
        args (rest expr)]
    (('add-to-stack env) kw funcname)
    (bind-args env kws args)))

(defn eval-fn-reduce

  ([env f context]
   (eval-fn-reduce env f context 0))

  ([env f context pop-n-times]
   
   "Reduces a lambda expression, with the option of binding
    to variables from a former level for continuations and closures."

   
   (let [args (rest (env/get-display env context))
         kws (-> env
                 (env/pop-stack pop-n-times)
                 (env/get-display (keyword (str (name f) "-args"))))]
     (-> env
         (env/pop-stack pop-n-times)
         (bind-args kws args)
         (eval-sub context f)))))

(defn eval-quote [env kw keep-quoted]
  (->> keep-quoted
       (refresh-symbol-vals-quote env kw)
       (str)
       (eval-str)
       (env/set-display env kw)))

(defn eval-sub [env kw sub]
  (env/set-display env kw (most-recent-kw env kw)))

(defn eval-pop [env kw]
  (-> env
      (env/pop-stack)
      (eval-reg kw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation Helpers

(defn bind-args [env kws args]
  
  "Binds the arguments of original function
  to the corresponding parameters of the function
  that it expands into."
  
  (let [tk (comp env/trim-kws name)]
    (doall (map #(env/set-display env (tk %1) %2)
                (vec kws)
                (vec args)))
    env))


(defn eval-str [s]
  
  "Workaround that permits eval in ClojureScript."
  
  (:value (eval (empty-state)
                (read-string s)
                {:eval       js-eval
                 :source-map true
                 :context    :expr}
                (fn [result] result))))

(defn refresh-symbol-vals [env kw]
  "Refreshes all symbol values."
  
  (let [display-map (env/get-display-map env)]
    (cycle-through-tree display-map env kw)))

(defn refresh-symbol-vals-quote [env kw exclude]
  "Refreshes all symbol values but one."
  
  (let [trimmed-kw (most-recent-kw env exclude)
        display-map (-> env
                        (env/get-display-map)
                        (dissoc trimmed-kw))]
    (cycle-through-tree display-map env kw)))

(defn cycle-through-tree [display-map env kw]

  "Recursively updates the values of each kw until all kws
   have been replaced with valid symbols."
  
  (let [kw-display (env/get-display env kw)]
    (loop [try-1 (w/postwalk-replace display-map kw-display)
           try-2 (w/postwalk-replace display-map try-1)]
      (if (= try-1 try-2)
        try-1
        (recur try-2 (w/postwalk-replace display-map try-2))))))


(defn most-recent-kw [env kw]
  "Append the stack number onto the key word."
  
  (env/append-kws kw (('get-stack-pos env))))
