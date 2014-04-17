(ns com.tbaldridge.slide
  (:require [com.tbaldridge.slide.util :as util]
            [clojure.core.async :refer [go <! >! chan timeout put! sliding-buffer map< pipe dropping-buffer]])
  (:import [javax.swing JFrame]
           [javafx.collections FXCollections]
           [javafx.event ActionEvent EventHandler]
           [javafx.scene Scene SceneBuilder]
           [javafx.scene.control Button ListView]
           [javafx.scene.layout StackPane]
           [javafx.stage Stage StageBuilder]
           [javafx.application Application]
           [javafx.beans.value ChangeListener ObservableValue]
           (javafx.scene.control ButtonBuilder TextFieldBuilder ListViewBuilder)
           (javafx.scene.layout VBoxBuilder)
           [java.lang.ref WeakReference]))

(def ^:dynamic *builder-mappings*
  {:stage StageBuilder
   :scene SceneBuilder
   :button ButtonBuilder
   :vbox VBoxBuilder
   :list-view ListViewBuilder
   :text-field TextFieldBuilder})

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
       (seq? v)) (FXCollections/observableArrayList v)

       :else v))

(defn output-binding? [nm]
  (.endsWith (name nm) "->"))

(defn input-binding? [nm]
  (.endsWith (name nm) "<-"))

(defn create-output-binding [crtl k v]
  (let [sk (name k)
        nm (subs sk 0 (- (count sk) 2))
        ctrl-prop (eval-call `(fn [ctrl#] (. ctrl# ~(symbol (str nm "Property")))) crtl)]
    (.addListener ctrl-prop
                  (reify ChangeListener
                    (changed [this cl old-val new-val]
                      (put! v new-val))))))

(defn create-input-binding [crtl k c]
  (let [sk (name k)
        nm (subs sk 0 (- (count sk) 2))
        ctrl-prop (eval-call `(fn [ctrl#] (. ctrl# ~(symbol (str nm "Property")))) crtl)]
        (go
          (try
            (loop []
              (when-let [v (<! c)]
                (println v)
                (util/run-later (.setValue ctrl-prop (to-javafx-value v)))
                (recur)))
            (catch Throwable ex
              (println ex))))))

(defn build-item [desc]
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

(defn build-scene [desc]
  (let [state (atom {})
        root (build-item state desc)]
    (doseq [[ctrl props] (:bind @state)
            [prop binds] props
            bind binds]
      (let [ctrl-prop (eval-call `(fn [ctrl#] (. ctrl# ~(symbol (str (name prop) "Property")))) @ctrl)
            c (complete-channel bind (:named @state))]
        (go
          (try
            (loop []
              (when-let [v (<! c)]
                (println v)
                (util/run-and-wait (.setValue ctrl-prop (to-javafx-value v)))
                (recur)))
            (catch Throwable ex
              (println ex))))))
    root))

(let [topics (atom {})]

  (defn publish [topic msg]
    (println topic msg)
    (let [subs (get @topics topic)]
      (doseq [sub subs]
        (put! sub msg))))

  (defn publish-to [topic]
    (let [c (chan (dropping-buffer 1024))]
      (go
        (loop []
          (when-let [v (<! c)]
            (publish topic v)
            (recur))))
      c))

  (defn subscribe-to [topic]
    (let [c (chan (dropping-buffer 1024))]
      (swap! topics update-in [topic] (fnil conj #{}) c)
      c))

  (defn get-topics []
    @topics))

(def scene {:type :stage
            :title "hello"
            :scene {:type :scene
                    :height 480
                    :width 640
                    :root {:type :vbox
                           :minHeight 400
                           :minWidth 400
                           :children [{:type :button
                                       :text<- (subscribe-to [:text])}
                                      {:type :text-field
                                       :name :input
                                       :text-> (publish-to [:text])
                                       :text "FooBar"}
                                      #_{:type :text-field
                                       :text (map-property
                                              (partial str "foo - ")
                                              (bind-to :input :text))}
                                      #_{:type :list-view
                                       :items (rand-list)}]}}})

(util/run-and-wait (.show (build-item scene)))

(println (get-topics))
