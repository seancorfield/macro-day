(ns day.macmac)

;; (make-synonym b binding) for example

(defmacro make-synonym [name orig]
  `(defmacro ~name [& ~'stuff]
    `(~'~orig ~@~'stuff)))