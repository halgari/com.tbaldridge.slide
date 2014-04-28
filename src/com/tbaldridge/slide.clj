(ns com.tbaldridge.slide
  (:require [com.tbaldridge.slide.util :as util]
            [clojure.core.async :refer [go <! >! >!! <!! chan timeout put! sliding-buffer map< pipe dropping-buffer] :as async]
            [clojure.java.io :as jio])
  (:import [javax.swing JFrame]
           [javafx.collections FXCollections]
           [javafx.event ActionEvent EventHandler]
           [javafx.scene Scene SceneBuilder GroupBuilder]
           [javafx.scene.shape CircleBuilder RectangleBuilder]
           [javafx.scene.text TextBuilder FontBuilder Font]
           [javafx.scene.control Button ListView Label ScrollPaneBuilder TabPaneBuilder TabBuilder]
           [javafx.scene.layout StackPaneBuilder BorderPane]
           [javafx.stage Stage StageBuilder]
           [javafx.application Application]
           [javafx.beans.value ChangeListener ObservableValue]
           (javafx.scene.control ButtonBuilder TextFieldBuilder ListViewBuilder TableViewBuilder TableColumnBuilder
                                 LabelBuilder)
           (javafx.scene.layout VBoxBuilder HBoxBuilder BorderPaneBuilder)
           [javafx.scene.chart PieChartBuilder PieChart$Data XYChartBuilder XYChart$Data XYChart$Series]
           [java.lang.ref WeakReference]))

(defn diff
  ([xs ys]
   (diff xs ys 0 0))
  ([[x & xs] [y & ys] xidx yidx]
   (if (nil? x)
     (when-not (nil? y)
       (cons {:op :add :idx yidx :val y}
             (lazy-seq (diff nil ys xidx (inc yidx)))))
     (if (= x y)
       (recur xs ys (inc xidx) (inc yidx))
       (if (nil? y)
         (cons {:op :remove :idx xidx}
               (lazy-seq (diff xs ys (inc xidx) (inc yidx))))
         (cons {:op :replace :idx xidx :val y}
               (lazy-seq (diff xs ys (inc xidx) (inc yidx)))))))))

(def ^:dynamic *builder-mappings*
  {:stage StageBuilder
   :scene SceneBuilder
   :group GroupBuilder
   :stack-pane StackPaneBuilder
   :circle CircleBuilder
   :text TextBuilder
   :font FontBuilder
   :rectangle RectangleBuilder
   :scroll-pane ScrollPaneBuilder
   :button ButtonBuilder
   :label LabelBuilder
   :vbox VBoxBuilder
   :hbox HBoxBuilder
   :border-pane BorderPaneBuilder
   :list-view ListViewBuilder
   :table-view TableViewBuilder
   :table-column TableColumnBuilder
   :text-field TextFieldBuilder
   :tab-pane TabPaneBuilder
   :tab TabBuilder
   :pie-chart PieChartBuilder
   :xy-chart XYChartBuilder})


(defn chan->ObservableList [c]
  (let [olist (FXCollections/observableArrayList)]
    (go (try (loop [old []]
               (let [v (<! c)]
                 (when-not (nil? v)
                   (let [diff-list (diff old v)]
                     (doseq [{:keys [op] :as dval} diff-list]
                       (case op
                         :add (.add olist (:val dval))
                         :remove (.remove olist (:idx dval))
                         :replace (.set olist (:idx dval) (:val dval))))
                     (recur v)))))
             (catch Throwable ex
               (println ex))))
    olist))


(defn load-font [^String name ^double size]
  (Font/loadFont name size))

(defonce force-toolkit-init
  (do (JFrame. "init")
      (javafx.embed.swing.JFXPanel.)))


(def cached-forms (atom {}))
(defn reset-cached-forms []
  (reset! cached-forms {}))

(defn eval-call [form & args]
  (let [f (if-let [v (get @cached-forms form)]
            v
            (-> cached-forms
                (swap! assoc form (eval form))
                (get form)))]
    (apply f args)))


(defn to-javafx-value [v]
  (cond
   (or (vector? v)
       (seq? v)) (FXCollections/observableArrayList (map to-javafx-value  v))

       :else v))


(defmulti data-converter identity)

(defmethod data-converter javafx.beans.property.SimpleStringProperty
  [x]
  (fn [data]
    (str data)))

(defmethod data-converter javafx.scene.Node$MiscProperties$8
  [x]
  (fn [data]
    (boolean data)))

(defmethod data-converter javafx.scene.control.ListView$2
  [x]
  (fn [data]
    (FXCollections/observableArrayList data)))

(defmethod data-converter :default
  [x]
  (fn [data]
    data))

(defn output-binding? [nm]
  (.endsWith (name nm) "->"))

(defn input-binding? [nm]
  (.endsWith (name nm) "<-"))

(defn create-output-binding [crtl k v]
  (let [sk (name k)
        nm (subs sk 0 (- (count sk) 2))
        ctrl-prop (eval-call `(fn [ctrl#] (. ctrl# ~(symbol (str nm "Property")))) crtl)]
    (.addListener ctrl-prop
                  (reify
                    ChangeListener
                    (changed [this cl old-val new-val]
                      (put! v (or new-val :nil)))
                    EventHandler
                    (handle [this event]
                      (put! v (or event :nil)))))))

(defn create-input-binding [crtl k c]
  (if (= (name k) "children<-")
    (go
      (try
        (loop []
          (when-let [v (<! c)]
            (util/run-later (-> crtl .getChildren (.setAll v)))
            (recur)))
        (catch Throwable ex
          (println ex))))
    (let [sk (name k)
          nm (subs sk 0 (- (count sk) 2))
          ctrl-prop (eval-call `(fn [ctrl#] (. ctrl# ~(symbol (str nm "Property")))) crtl)
          converter (data-converter (class ctrl-prop))]
      (go
        (try
          (loop []
            (when-let [v (<! c)]
              (util/run-later (.setValue ctrl-prop (converter v)))
              (recur)))
          (catch Throwable ex
            (println ex)))))))

(defmulti build-item :type)

(defmethod build-item :default
  [desc]
  (if (map? desc)
    (let [builder (get *builder-mappings* (:type desc))
          _ (assert builder (str "Could not find builder for " (pr-str desc)))
          bldr (eval-call `(fn [] (. ~builder create)))
          ctrl-promise (promise)]
      (doseq [[k v] (dissoc desc :type :name)]
        (when-let [v (cond
                      (output-binding? k) nil
                      (input-binding? k) nil
                      (map? v) (build-item v)
                      (vector? v) (mapv build-item v)
                      :else v)]
          (eval-call `(fn [x# v#] (. x# ~(symbol (name k)) v#)) bldr (to-javafx-value v))))
      (let [built (.build bldr)]
        (doseq [[k v] (dissoc desc :type :name)]
          (cond
           (output-binding? k) (create-output-binding built k v)
           (input-binding? k) (create-input-binding built k v)
           :else nil))        
        built))
    desc))

(defmethod build-item :pie-chart-data
           [{:keys [name value]}]
  (PieChart$Data. name value))

(defmethod build-item :xy-chart-series
           [{:keys [name data]}]
  (let [series (XYChart$Series.)]
    (.setName series name)
    (doseq [itm data]
      (-> series .getData (.add (build-item itm))))
    series))

(defmethod build-item :xy-chart-data
           [{:keys [x y]}]
  (XYChart$Data. x y))

(let [topics (atom {})]

  (defn publish [topic msg]
    (let [subs (get @topics topic)]
      (doseq [sub subs]
        (when-let [c (.get sub)]
          (put! c msg)))))

  (defn publish-to [topic]
    (let [c (chan (dropping-buffer 1024))]
      (go
        (try
          (loop []
            (if-let [v (<! c)]
              (do (publish topic v)
                  (recur))
              (println "got nil")))
          (catch Throwable ex
            (println ex))))
      c))

  (defn subscribe-to [topic]
    (let [c (chan (dropping-buffer 1024))]
      (swap! topics update-in [topic] (fnil conj #{}) (WeakReference. c))
      c))

  (defn get-topics []
    @topics))


(defn bind-to-get-in [a path]
  (let [c (chan (dropping-buffer 1))]
    (add-watch a c (fn [k r o n]
                     (when-let [v (get-in @a path)]
                       (if (not (put! c v))
                         (remove-watch a c)))))
    (when-let [v (get-in @a path)]
      (put! c v))
    (async/unique c)))

(defn bind-to-assoc-in [a path]
  (let [c (chan (async/sliding-buffer 1))]
    (go (try (loop []
               (let [v (<! c)]
                 (when-not (nil? v)
                   (swap! a assoc-in path v)
                   (recur))))
             (catch Throwable ex
               (println ex))))
    c))

(comment
  (def a (atom {:foo "1"
                :disable false}))

  (def scene {:type :stage
              :title "hello"
              :scene {:type :scene
                      :height 480
                      :width 640
                      :root {:type :vbox
                             :minHeight 400
                             :minWidth 400
                             :children [{:type :button
                                         :text<- (bind-to-get-in a [:foo])
                                         :pressed-> (publish-to :button-clicked2)}
                                        {:type :label
                                         :text "Sup homie?"}
                                        {:type :text-field
                                         :name :input
                                         :text-> (bind-to-assoc-in a [:foo])
                                         :text "FooBar"}
                                        {:type :border-pane
                                         :top {:type :button
                                               :text "Hey"}
                                         :center {:type :table-view
                                                  :columns [{:type :table-column
                                                             :text "name"}
                                                            {:type :table-column
                                                             :text "age"}]
                                                  :items [["foo" "bar"]]}
                                         :left {:type :button
                                                :text "cool"}
                                         :right {:type :button
                                                 :text "right on"}}
                                        #_{:type :text-field
                                           :text (map-property
                                                  (partial str "foo - ")
                                                  (bind-to :input :text))}
                                        #_{:type :list-view
                                           :items (rand-list)}]}}})

  (util/run-and-wait (.show (build-item scene)))

  (swap! a assoc :items ["fo2" "bar"])
  (println @a)
  )
