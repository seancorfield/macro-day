(ns day.anaphora)

;; ~'x => capture x as variable

#_(anaphoric-if (some-computation 11)
              (* 2 it))

(defn some-computation [x] (if (even? x) false (inc x)))

(defmacro anaphoric-if [expr & body]
  `(if-let [~'it ~expr] (do ~@body)))

(defmacro anaphoric-and
  ([] true)
  ([x] x)
  ([x & more] 
    `(anaphoric-if ~x 
                   (anaphoric-and ~@more))))

(defmacro with-it [f a & more]
  `(let [~'it ~a]
     (~f ~'it ~@more)))

(defmacro a-if [a & more]
  `(with-it if ~a ~@more))

(defmacro thread-it 
  ([x] x)
  ([x & more] 
    `(let [~'it ~x]
       (thread-it ~@more))))