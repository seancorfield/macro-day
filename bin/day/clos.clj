(ns day.clos)

(def this)

(defn method-call [klass m-name args]
  (if klass
    (let [m (m-name (klass :methods))]
      (if m
        (apply m args)
        (method-call (klass :parent) m-name args)))
    (throw (RuntimeException. (str "Unable to find method named " m-name)))))

(defn new-object [klass]
  (let [state (atom {})]
    (fn thiz [c & args]
      (binding [this thiz] 
        (condp = c
          :class klass
          :class-name (klass :name)
          :set! (let [[k v] args]
                  (dosync
                    (alter state assoc k v)))
          :get (let [[k] args]
                 (state k))
          (method-call klass c args))))))

(defn new-class [class-name methods parent]
  (fn thiz [c & args]
    (condp = c
      :name (name class-name)
      :new (new-object thiz)
      :methods methods
      :parent parent)))

(defn method-spec [[mname args & body]]
  `{~(keyword mname) (fn ~args ~@body)})

(defmacro defclass [c-name & body]
  (let [m-specs (map rest (filter #(= (first %) 'method) body))
        methods (apply merge (map method-spec m-specs))
        parent (last (apply merge (filter #(= (first %)'extends) body)))]
    `(def ~c-name (new-class (keyword '~c-name) ~methods ~parent))))

;; examples

(defclass Animal
  (method make-sound [] (str "SOUND!")))

(defclass Person
  (extends Animal)
  (method say-hello [n] (str "Hello " n "!")))

(def siva (Person :new))

(siva :class-name)

(siva :make-sound)

(siva :say-hello "Sean")