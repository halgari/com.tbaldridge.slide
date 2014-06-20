(ns com.tbaldridge.slide.bindings
  (:require [com.tbaldridge.slide.util :as util]
            [clojure.core.async :refer [go <! >! >!! <!! chan timeout put! sliding-buffer map< pipe dropping-buffer] :as async])
  (:import [java.lang.ref WeakReference]))


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



(defn swap-old! [a f & args]
  (loop []
    (let [old @a
          new (apply f old args)]
      (if (compare-and-set! a old new)
        old
        (recur)))))


(defprotocol IClearable
  (when-freed [this f]))

(defn weak-ref [val]
  (let [ref (WeakReference. val)
        a (atom [])]
    (reify
      clojure.lang.IDeref
      (deref [this]
        (let [v (.get ref)]
          (when (nil? v)
            (let [bcs (swap-old! a empty)]
              (doseq [cb bcs]
                (cb))))
          v))
      IClearable
      (when-freed [this f]
        (swap! a conj f)
        @this))))

(defn get-in! [a path]
  (let [c (chan (dropping-buffer 1))
        cref (weak-ref c)]
    (add-watch a c (fn [k r o n]
                     (let [v (get-in @a path)
                           chn @cref]
                       (when (and (not (nil? v))
                                  chn)
                         (if (not (put! chn v))
                           (remove-watch a chn))))))
    (when-freed cref (partial remove-watch a c))
    (when-let [v (get-in @a path)]
      (put! c v))
    (async/unique c)))

(defn assoc-in! [a path]
  (let [c (chan (async/sliding-buffer 1))]
    (go (try (loop []
               (let [v (<! c)]
                 (when-not (nil? v)
                   (swap! a assoc-in path v)
                   (recur))))
             (catch Throwable ex
               (println ex))))
    c))
