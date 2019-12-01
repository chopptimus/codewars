(ns codewars.caesar2
  (:require [clojure.string :refer [lower-case]]
            [clojure.test :refer [deftest testing is]]))

(defn encode [shift c]
  (let [i (int c)]
    (if (and (> i 64) (< i 123))
      (char (+ i shift))
      c)))

(defn prefix [c shift]
  (let [i (int (first (lower-case c)))]
    (list (char i) (char (+ i shift)))))

(defn encode-str
  [s shift]
  (let [m (concat (prefix (first s) shift) (map (partial encode shift) s))
        n (quot (+ (count m) 4) 5)]
    (vec (map (partial apply str) (partition n n [] m)))))

(defn prefix->shift [s]
  (apply - (map int (reverse s))))

(defn decode [ss]
  (let [s (apply str ss)
        shift (- (prefix->shift (take 2 s)))]
    (apply str (map (partial encode shift) (drop 2 s)))))

(deftest a-test1
  (testing "Test1"
    (def u "I should have known that you would have a perfect answer for me!!!")
    (def v ["ijJ tipvme ibw","f lopxo uibu z","pv xpvme ibwf ","b qfsgfdu botx","fs gps nf!!!"])
    (is (= (encode-str u 1) v))))
(deftest a-test2
  (testing "Test2"
    (def u "How can we become the kind of people that face our fear and do it anyway?")
    (def v ["hiIpx dbo xf cf","dpnf uif ljoe p","g qfpqmf uibu g","bdf pvs gfbs bo","e ep ju bozxbz?"])
    (is (= (encode-str u 1) v))))
