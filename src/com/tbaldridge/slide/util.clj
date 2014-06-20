(ns com.tbaldridge.slide.util
  (:import [javafx.application Platform]))


(defn run-later*
  [^Runnable f]
  (Platform/runLater f))

(defmacro run-later
  [& body]
  `(run-later* (fn [] ~@body)))

(defn run-and-wait*
  [f]
  (let [result (promise)]
    (run-later
     (deliver result (try (f) (catch Throwable e
                                (println e)
                                e))))
    (if (instance? Throwable @result)
      (throw @result)
      @result)))

(defmacro run-and-wait
  [& body]
  `(run-and-wait* (fn [] ~@body)))

