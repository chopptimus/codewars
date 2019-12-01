(ns codewars.smallest-sum)

; (defn bisect
;   [coll x]
;   (if (seq coll)
;     (loop [lo 0 hi (count coll)]
;       (if (= (nth coll (/ (+ lo hi) 2)) x)
;         ))
;     0))

(defn euclid
  [a b]
  (let [rem (mod a b)]
    (if (zero? rem)
      b
      (recur b rem))))

(defn solution
  [arr]
  (* (reduce euclid 0 arr) (count arr)))

(comment

  (def arr [6 9 21])

  (solution arr)

  )
