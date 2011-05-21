(ns day.core)

;; cannot define unless as a function because args
;; are evaluated eagerly, not lazily...
(defmacro unless [p body]
  `(if ~p nil ~body))

(defmacro unless-amit [expr form]
  (list 'if expr nil form))

(defmacro unless-do [expr & forms]
  `(if ~expr nil (do ~@forms)))

(defmacro foo [ & f]
  `(let [a# (inc 1)]
     (println 'a# 'f ~@f)))

(defmacro my-declare [ & vars]
  `(do ~@(map #(list 'def %) vars)))

(defmacro my-declare-2 [ & vars]
  `(do ~@(for [v# vars] (list 'def v#))))

(defn def-form [x]
  `(def ~x))

(defmacro my-declare-1 [ & vars]
  `(do ~@(map def-form vars)))

(defmacro infix [a op b]
  `(~op ~a ~b))