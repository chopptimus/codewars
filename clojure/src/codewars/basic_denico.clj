(ns codewars.basic-denico
  (:require [clojure.string :refer [trim]]))

(defn string->key [k]
  (->> (map vector k (range))
       sort
       (map vector (range))
       (sort-by (comp second second))
       (map first)))

(defn reorder-chunk
  [ordering ch]
  (map (partial get (vec ch)) ordering))

(defn denico
  [key-string cipher]
  (let [k (string->key key-string)
        n (count k)]
    (->> cipher
         (partition n n (repeat \space))
         (mapcat (partial reorder-chunk k))
         (apply str)
         trim)))
