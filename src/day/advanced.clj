(ns day.advanced)

#_ (my-let [x 10
            y x
            z (+ x y)]
           (* x y z))

(defmacro single-arg-fn [[v expr] & body]
  `((fn [~v] ~@body) ~expr))

(defmacro my-let [lettings & body]
  (let [helper (fn
          ([] `(do ~@body))
          ([v expr & more]
            `(single-arg-fn [~v ~expr]
                            (my-let ~more ~@body))))]
    (apply helper lettings)))

;; (extend-fn f [x y z] (* x y z))
(defmacro extend-fn [name args & body]
  `(let [old-fn# (var-get (var ~name))
         new-fn# (fn ~args (do ~@body))
         dispatcher# (fn [& more#]
                       (if (= ~(count args) (count more#))
                         (apply new-fn# more#)
                         (apply old-fn# more#)))]
     (alter-var-root (var ~name) (constantly dispatcher#))))
