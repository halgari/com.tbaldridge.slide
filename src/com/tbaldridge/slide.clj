(ns com.tbaldridge.slide
  (:import (java.util WeakHashMap))
  (:require [com.tbaldridge.slide.util :as util]
            [clojure.core.async :refer [go <! >! >!! <!! chan timeout put! sliding-buffer map< pipe dropping-buffer] :as async]
            [clojure.java.io :as jio]
            [clojure.reflect :as reflect]
            [clojure.data :as data])
  (:import (javax.swing JFrame)
           (javafx.collections FXCollections)
           (javafx.event ActionEvent EventHandler)
           (javafx.scene Scene SceneBuilder GroupBuilder)
           (javafx.scene.shape CircleBuilder RectangleBuilder)
           (javafx.scene.text TextBuilder FontBuilder Font)
           (javafx.scene.control ScrollPaneBuilder TabPaneBuilder TabBuilder)
           (javafx.scene.layout StackPaneBuilder BorderPane)
           (javafx.stage Stage StageBuilder)
           (javafx.application Application)
           (javafx.beans.value ChangeListener ObservableValue)
           (javafx.scene.control ButtonBuilder TextFieldBuilder ListViewBuilder TableViewBuilder TableColumnBuilder
                                 LabelBuilder)
           (javafx.scene.layout VBoxBuilder HBoxBuilder BorderPaneBuilder)
           (javafx.scene.chart PieChartBuilder PieChart$Data XYChartBuilder XYChart$Data XYChart$Series LineChartBuilder NumberAxis
                               BarChartBuilder)
           (javafx.embed.swing JFXPanel)
           (java.util WeakHashMap)))


(JFXPanel.)
(def convert-value nil)
(defmulti convert-value (fn [dt old new]
                          dt))

(defprotocol IUIComponent
  (update-property [this k v]))


(defprotocol IComponentRenderer
  (-render [this])
  (-get-data [this]))


(def data-table (WeakHashMap.))
(def state-table (WeakHashMap.))

(def construct-component nil)
(defmulti construct-component (fn [data state]
                                (:type state)))


(defn make-component [f data]
  (reify IComponentRenderer
    (-render [this]
      (f data))
    (-get-data [this]
      data)))

(defmacro component [[data] & body]
  (assert (symbol? data))
  `(make-component
     (fn [~data]
       ~@body)
     ~data))

(defn convert-name [nm]
  (loop [c (seq nm)
         acc []
         last-upper true]
    (let [[h & t] c]
      (if h
        (if (Character/isUpperCase h)
          (if last-upper
            (recur t (conj acc (Character/toLowerCase h)) true)
            (recur t (conj acc \- (Character/toLowerCase h)) true))
          (recur t (conj acc h) false))
        (apply str acc)))))

(defn de-dup-cases [cases]
  (first (reduce
           (fn [[cases seen :as acc] [kw :as case]]
             (if (seen kw)
               acc
               [(conj cases case)
                (conj seen kw)]))
           [[] #{}]
           cases)))

(defn properties-for-class [klass]
  (let [klass (Class/forName (name klass))]
    (->> (for [member (:members (reflect/type-reflect klass :ancestors true))
               :let [member-name (name (:name member))]
               :when (.startsWith member-name "set")
               :let [getter-name (symbol (str "get" (subs member-name 3)))]]
           `[~(keyword (convert-name (subs member-name (count "set"))))
             (if (nil? ~'value)
               (. ~'this ~(:name member) nil)
               (let [old# (. ~'this ~getter-name)
                     v# (convert-value ~(first (:parameter-types member))
                                       old# ~'value)]
                 (if (identical? old# v#)
                   nil
                   (. ~'this ~(:name member) v#))))])
         de-dup-cases
         (sort-by first)
         (apply concat))))

(clojure.pprint/pprint (properties-for-class 'javafx.stage.Stage))

(defn update-properties [component old-state new-state]
  (let [[old-diff new-diff] (data/diff old-state new-state)]
    (println "states " old-diff new-diff)
    (doseq [[k v] (dissoc new-diff :type)]
      (update-property component k v))
    (doseq [[k] (apply dissoc old-diff :type (keys new-diff))]
      (update-property component k nil))
    component))

(defn to-lower-name [nm]
  (let [nm (name nm)
        idx (.lastIndexOf nm ".")]
    (symbol (convert-name (.toLowerCase (subs nm (inc idx)))))))

(defn builder-name [nm]
  (symbol (str (name nm) "Builder")))

(deftype Component [data f statics]
  IComponentRenderer
  (-get-data [this]
    data)
  (-render [this]
    (apply f data statics))
  Object
  (hashCode [this]
    (hash data))
  (equals [this other]
    (and (instance? Component other)
         (= data (-get-data other)))))

(defmacro defcomponent [tp]
  `(do (defmethod construct-component ~tp
                     [in-data# in-state#]
         (let [comp# (.build (. ~(builder-name tp) ~'create))]
           (.put data-table comp# in-data#)
           (.put state-table comp# in-state#)
           (update-properties comp# nil in-state#)))
       (defn ~(to-lower-name tp) [& {:as opts#}]
         (Component. (assoc opts# :type ~tp)
                     identity
                     nil))
       (extend-type ~tp
         IUIComponent
         (update-property [~'this k# ~'value]
           (println "update " ~'this k# ~'value)
           (case k#
             ~@(properties-for-class tp))))))

(defcomponent javafx.stage.Stage)
(defcomponent javafx.scene.Scene)
(defcomponent javafx.scene.control.Button)
(defcomponent javafx.scene.layout.StackPane)

(defmethod construct-component javafx.scene.Scene
           [in-data in-state]
  (let [comp# (.build (-> (. javafx.scene.SceneBuilder create)
                          (.root (javafx.scene.layout.StackPane.))))]
    (.put data-table comp# in-data)
    (.put state-table comp# in-state)
    (update-properties comp# nil in-state)))

(defn log [x]
  (println "------<> " x)
  x)

(defn render-all [component]
  (println "render..." component)
  (if (instance? Component component)
    (recur (-render component))
    component))

(defn render [component new-component]
  (println (-get-data new-component))
  (log (if-not component
         [:new (construct-component (-get-data new-component)
                                    (render-all new-component))]
         (let [old-data (.get data-table component)]
           (if (= old-data (-get-data new-component))
             [:nil component]
             (let [old-state (.get state-table component)
                   new-state (render-all new-component)]
               (if (instance? (:type new-state) component)
                 (do
                   (update-properties component old-state new-state)
                   (.put state-table component new-state)
                   (.put data-table component (-get-data new-component))
                   [:updated component])
                 [:replace (construct-component (-get-data new-component)
                                                (render-all new-component))])))))))

(defmethod convert-value javafx.scene.Scene
           [tp old val]
  (let [[action val] (render old val)]
    (println action val)
    (if action
      val
      old)))

(defmethod convert-value java.lang.String
           [tp old val]
  (assert (string? val))
  val)

(defmethod convert-value javafx.scene.Parent
           [tp old val]
  (let [[action val] (render old val)]
    (println action val)
    (if action
      val
      old)))

(defmethod convert-value javafx.event.EventHandler
           [tp old new-val]
  (assert (var? new-val))
  (reify javafx.event.EventHandler
    (handle [this event]
      (new-val event))))

(defn sup-component [txt]
  (Component. txt
              (fn [txt]
                (button :text txt))
              nil))

(let [a (atom nil)]
  (util/run-later
    (reset! a (second (render @a (stage :title "foo"
                                        :scene (scene :root (sup-component "foo"))))))
    #_(reset! a (second (render @a (stage :title "bar"
                                        :scene (scene :root (button :text "sup?"))))))
    (println "....")
    (reset! a (second (render @a (stage :title "bar"
                                        :scene (scene :root (sup-component "foo"))))))
    (.show @a)
    (println "done")
    ))
