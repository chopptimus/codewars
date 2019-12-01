(ns codewars.sum-intervals)

(defn sum-intervals
  [itvls]
  (let [itvls (sort itvls)]
    (loop [itvls itvls merged '()]
      (if (> (count itvls) 1)
        (let [[[a1 b1] [a2 b2]] itvls]
          (if (> b1 a2)
            (recur (cons [a1 (max b1 b2)] (rest (rest itvls))) merged)
            (recur (rest itvls) (cons (first itvls) merged))))
        (->> (into itvls merged)
             (map (fn [[a b]] (- b a)))
             (reduce + 0))))))

(comment

  (def itvls [[-4 11] [-3 -1] [1 4] [3 8] [7 10]])

  (sum-intervals itvls)

  )
