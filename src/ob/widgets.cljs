(ns ^:figwheel-hooks ob.widgets
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
