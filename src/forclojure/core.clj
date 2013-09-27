(ns forclojure.core
  (:require [clojure.set]))

(defn problem1 []
  ;(= __ true)
  (= true true))

(defn problem2 []
  ;(= (- 10 (* 2 3)) __)
  (= (- 10 (* 2 3)) 4))

(defn problem3 []
 ;(= __ (.toUpperCase "hello world"))
  (= "HELLO WORLD" (.toUpperCase "hello world")))

(defn problem4 []
  ;(= (list __) '(:a :b :c))
  (= (list :a :b :c) '(:a :b :c)))

(defn problem5 []
  (comment
    (= __ (conj '(2 3 4) 1))
    (= __ (conj '(3 4) 2 1)))
  (let [x (list 1 2 3 4)]
    (and
     (= x (conj '(2 3 4) 1))
     (= x (conj '(3 4) 2 1)))))

(defn problem6 []
  ;(= [__] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))
  (= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c)))

(defn problem7 []
  (comment
   (= __ (conj [1 2 3] 4))
   (= __ (conj [1 2] 3 4)))
  (let [x [1 2 3 4]]
    (and
     (= x (conj [1 2 3] 4))
     (= x (conj [1 2] 3 4)))))

(defn problem8 []
  (comment
    (= __ (set '(:a :a :b :c :c :c :c :d :d)))
    (= __ (clojure.set/union #{:a :b :c} #{:b :c :d})))
  (let [x #{:a :b :c :d}]
    (and
     (= x (set '(:a :a :b :c :c :c :c :d :d)))
     (= x (clojure.set/union #{:a :b :c} #{:b :c :d})))))

(defn problem9 []
  ;(= #{1 2 3 4} (conj #{1 4 3} __))
  (= #{1 2 3 4} (conj #{1 4 3} 2)))

(defn problem10 []
  (comment
    (= __ ((hash-map :a 10, :b 20, :c 30) :b))
    (= __ (:b {:a 10, :b 20, :c 30})))
  (let [x 20]
    (and
     (= x ((hash-map :a 10, :b 20, :c 30) :b))
     (= x (:b {:a 10, :b 20, :c 30})))))

(defn problem11 []
  ;(= {:a 1, :b 2, :c 3} (conj {:a 1} __ [:c 3]))
  (= {:a 1, :b 2, :c 3} (conj {:a 1} {:b 2} [:c 3])))

(defn problem12 []
  (comment
    (= __ (first '(3 2 1)))
    (= __ (second [2 3 4]))
    (= __ (last (list 1 2 3))))
  (let [x 3]
    (and
     (= x (first '(3 2 1)))
     (= x (second [2 3 4]))
     (= x (last (list 1 2 3))))))

(defn problem13 []
  ;(= __ (rest [10 20 30 40]))
  (= [20 30 40] (rest [10 20 30 40])))

(defn problem14 []
  (comment
    (= __ ((fn add-five [x] (+ x 5)) 3))
    (= __ ((fn [x] (+ x 5)) 3))
    (= __ (#(+ % 5) 3))
    (= __ ((partial + 5) 3)))
  (let [x 8]
    (and
     (= x ((fn add-five [x] (+ x 5)) 3))
     (= x ((fn [x] (+ x 5)) 3))
     (= x (#(+ % 5) 3))
     (= x ((partial + 5) 3)))))

(defn problem15 []
  (comment
    (= (__ 2) 4)
    (= (__ 3) 6)
    (= (__ 11) 22)
    (= (__ 7) 14))
  (letfn [(double [x] (* x 2))]
    (and
     (= (double 2) 4)
     (= (double 3) 6)
     (= (double 11) 22)
     (= (double 7) 14))))

(defn problem16 []
  (comment
    (= (__ "Dave") "Hello, Dave!")
    (= (__ "Jenn") "Hello, Jenn!")
    (= (__ "Rhea") "Hello, Rhea!"))
  (letfn [(hello [x] (str "Hello, " x "!"))]
    (and
     (= (hello "Dave") "Hello, Dave!")
     (= (hello "Jenn") "Hello, Jenn!")
     (= (hello "Rhea") "Hello, Rhea!"))))

(defn problem17 []
  ;(= __ (map #(+ % 5) '(1 2 3)))
  (= [6 7 8] (map #(+ % 5) '(1 2 3))))

(defn problem18 []
  ;(= __ (map #(+ % 5) '(1 2 3)))
  (= [6 7] (map #(+ % 5) '(1 2 3))))

(defn problem19 []
  (comment
    (= (__ [1 2 3 4 5]) 5)
    (= (__ '(5 4 3)) 3)
    (= (__ ["b" "c" "d"]) "d"))
  (letfn [(my-last [x] (first (reverse x)))]
    (and
     (= (my-last [1 2 3 4 5]) 5)
     (= (my-last '(5 4 3)) 3)
     (= (my-last ["b" "c" "d"]) "d"))))

(defn problem20 []
  (comment
    (= (__ (list 1 2 3 4 5)) 4)
    (= (__ ["a" "b" "c"]) "b")
    (= (__ [[1 2] [3 4]]) [1 2]))
  (letfn [(penultimate [x] (second (reverse x)))]
    (and
     (= (penultimate (list 1 2 3 4 5)) 4)
     (= (penultimate ["a" "b" "c"]) "b")
     (= (penultimate [[1 2] [3 4]]) [1 2]))))

(defn problem21 []
  (comment
    (= (__ '(4 5 6 7) 2) 6)
    (= (__ [:a :b :c] 0) :a)
    (= (__ [1 2 3 4] 1) 2)
    (= (__ '([1 2] [3 4] [5 6]) 2) [5 6]))
  (letfn [(my-nth [c n] (first (drop n c)))]
    (and
     (= (my-nth '(4 5 6 7) 2) 6)
     (= (my-nth [:a :b :c] 0) :a)
     (= (my-nth [1 2 3 4] 1) 2)
     (= (my-nth '([1 2] [3 4] [5 6]) 2) [5 6]))))

(defn problem22 []
  (comment
    (= (__ '(1 2 3 3 1)) 5)
    (= (__ "Hello World") 11)
    (= (__ [[1 2] [3 4] [5 6]]) 3)
    (= (__ '(13)) 1)
    (= (__ '(:a :b :c)) 3))
  (letfn [(my-count [c] (reduce (fn [n coll] (inc n)) 0 c))]
    (and
     (= (my-count '(1 2 3 3 1)) 5)
     (= (my-count "Hello World") 11)
     (= (my-count [[1 2] [3 4] [5 6]]) 3)
     (= (my-count '(13)) 1)
     (= (my-count '(:a :b :c)) 3))))

(defn problem23 []
  (comment
    (= (__ [1 2 3 4 5]) [5 4 3 2 1])
    (= (__ (sorted-set 5 7 2 7)) '(7 5 2))
    (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]]))
  (letfn [(my-rev [c] (reduce (fn [v c] (cons c v)) [] c))]
    (and
     (= (my-rev [1 2 3 4 5]) [5 4 3 2 1])
     (= (my-rev (sorted-set 5 7 2 7)) '(7 5 2))
     (= (my-rev [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]]))))

(defn problem24 []
  (comment
    (= (__ [1 2 3]) 6)
    (= (__ (list 0 -2 5 5)) 8)
    (= (__ #{4 2 1}) 7)
    (= (__ '(0 0 -1)) -1)
    (= (__ '(1 10 3)) 14))
  (letfn [(sum [c] (reduce + c))]
    (and
     (= (sum [1 2 3]) 6)
     (= (sum (list 0 -2 5 5)) 8)
     (= (sum #{4 2 1}) 7)
     (= (sum '(0 0 -1)) -1)
     (= (sum '(1 10 3)) 14))))

(defn problem25 []
  (comment
    (= (__ #{1 2 3 4 5}) '(1 3 5))
    (= (__ [4 2 1 6]) '(1))
    (= (__ [2 2 4 6]) '())
    (= (__ [1 1 1 3]) '(1 1 1 3)))
  (letfn [(filter-odd [c] (filter odd? c))]
    (and
     (= (filter-odd #{1 2 3 4 5}) '(1 3 5))
     (= (filter-odd [4 2 1 6]) '(1))
     (= (filter-odd [2 2 4 6]) '())
     (= (filter-odd [1 1 1 3]) '(1 1 1 3)))))

(defn problem26 []
  (comment
    (= (__ 3) '(1 1 2))
    (= (__ 6) '(1 1 2 3 5 8))
    (= (__ 8) '(1 1 2 3 5 8 13 21)))
  (letfn [(n-fib [n] (take n (map first
                                  (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))]
    (and
     (= (n-fib 3) '(1 1 2))
     (= (n-fib 6) '(1 1 2 3 5 8))
     (= (n-fib 8) '(1 1 2 3 5 8 13 21)))))


(defn problem27 []
  (comment
    (false? (__ '(1 2 3 4 5)))
    (true? (__ "racecar"))
    (true? (__ [:foo :bar :foo]))
    (true? (__ '(1 1 3 3 1 1)))
    (false? (__ '(:a :b :c))))
  (letfn [(palindrome? [c] (= (reverse c) (seq c)))]
    (and
     (false? (palindrome? '(1 2 3 4 5)))
     (true? (palindrome? "racecar"))
     (true? (palindrome? [:foo :bar :foo]))
     (true? (palindrome? '(1 1 3 3 1 1)))
     (false? (palindrome? '(:a :b :c))))))

(defn problem28 []
  (comment
    (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
    (= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
    (= (__ '((((:a))))) '(:a)))
  (letfn [(my-flatten [c] (filter (complement sequential?)
                                  (rest (tree-seq sequential? seq c))))]
    (and
     (= (my-flatten '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
     (= (my-flatten ["a" ["b"] "c"]) '("a" "b" "c"))
     (= (my-flatten '((((:a))))) '(:a)))))

(defn problem29 []
  (comment
    (= (__ "HeLlO, WoRlD!") "HLOWRD")
    (empty? (__ "nothing"))
    (= (__ "$#A(*&987Zf") "AZ"))
  (letfn [(capitals [s] (apply str (re-seq #"[A-Z]+" s)))]
    (and
     (= (capitals "HeLlO, WoRlD!") "HLOWRD")
     (empty? (capitals "nothing"))
     (= (capitals "$#A(*&987Zf") "AZ"))))

(defn problem30 []
  (comment
    (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
    (= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
    (= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))
  (letfn [(rm-dup [x] (map first (partition-by identity x)))]
    (and
     (= (apply str (rm-dup "Leeeeeerrroyyy")) "Leroy")
     (= (rm-dup [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
     (= (rm-dup [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))))

(defn problem31 []
  (comment
    (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
    (= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
    (= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))
  (letfn [(pack-seq [c] (partition-by identity c))]
    (and
     (= (pack-seq [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
     (= (pack-seq [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
     (= (pack-seq [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))))

(defn problem32 []
  (comment
    (= (__ [1 2 3]) '(1 1 2 2 3 3))
    (= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
    (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
    (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
  (letfn [(dup-seq [c] (interleave c c))]
    (and
     (= (dup-seq [1 2 3]) '(1 1 2 2 3 3))
     (= (dup-seq [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
     (= (dup-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
     (= (dup-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))))

(defn problem33 []
  (comment
    (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
    (= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
    (= (__ [4 5 6] 1) '(4 5 6))
    (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
    (= (__ [44 33] 2) [44 44 33 33]))
  (letfn [(rep-seq [c n] (mapcat (partial repeat n) c))]
    (and
     (= (rep-seq [1 2 3] 2) '(1 1 2 2 3 3))
     (= (rep-seq [:a :b] 4) '(:a :a :a :a :b :b :b :b))
     (= (rep-seq [4 5 6] 1) '(4 5 6))
     (= (rep-seq [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
     (= (rep-seq [44 33] 2) [44 44 33 33]))))

(defn problem34 []
  (comment
    (= (__ 1 4) '(1 2 3))
    (= (__ -2 2) '(-2 -1 0 1))
    (= (__ 5 8) '(5 6 7)))
  (letfn [(my-range [s e] (take (- e s) (iterate inc s)))]
    (and
     (= (my-range 1 4) '(1 2 3))
     (= (my-range -2 2) '(-2 -1 0 1))
     (= (my-range 5 8) '(5 6 7)))))

(defn problem35 []
  (comment
    (= __ (let [x 5] (+ 2 x)))
    (= __ (let [x 3, y 10] (- y x)))
    (= __ (let [x 21] (let [y 3] (/ x y)))))
  (let [x 7]
    (and
     (= x (let [x 5] (+ 2 x)))
     (= x (let [x 3, y 10] (- y x)))
     (= x (let [x 21] (let [y 3] (/ x y)))))))

(defn problem36 []
  (comment
    (= 10 (let __ (+ x y)))
    (= 4 (let __ (+ y z)))
    (= 1 (let __ z)))
  (and
   (= 10 (let [x 7 y 3 z 1] (+ x y)))
   (= 4 (let [x 7 y 3 z 1] (+ y z)))
   (= 1 (let [x 7 y 3 z 1] z))))

(defn problem37 []
  ;(= __ (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))
  (= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))

(defn problem38 []
  (comment
    (= (__ 1 8 3 4) 8)
    (= (__ 30 20) 30)
    (= (__ 45 67 11) 67))
  (letfn [(my-max [& more] (last (sort more)))]
    (and
     (= (my-max 1 8 3 4) 8)
     (= (my-max 30 20) 30)
     (= (my-max 45 67 11) 67))))

(defn problem39 []
  (comment
    (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
    (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
    (= (__ [1 2 3 4] [5]) [1 5])
    (= (__ [30 20] [25 15]) [30 25 20 15]))
  (letfn [(my-interleave [a b] (mapcat vector a b))]
    (and
     (= (my-interleave [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
     (= (my-interleave [1 2] [3 4 5 6]) '(1 3 2 4))
     (= (my-interleave [1 2 3 4] [5]) [1 5])
     (= (my-interleave [30 20] [25 15]) [30 25 20 15]))))

(defn problem40 []
  (comment
    (= (__ 0 [1 2 3]) [1 0 2 0 3])
    (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
    (= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))
  (letfn [(my-interpose [v c] (drop-last (interleave c (repeat v))))]
    (and
     (= (my-interpose 0 [1 2 3]) [1 0 2 0 3])
     (= (apply str (my-interpose ", " ["one" "two" "three"])) "one, two, three")
     (= (my-interpose :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))))

(defn problem41 []
  (comment
    (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
    (= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
    (= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6]))
  (letfn [(drop-every-n [c n] (flatten (map (partial take (dec n))
                                            (partition-all n c))))]
    (and
     (= (drop-every-n [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
     (= (drop-every-n [:a :b :c :d :e :f] 2) [:a :c :e])
     (= (drop-every-n [1 2 3 4 5 6] 4) [1 2 3 5 6]))))

(defn problem42 []
  (comment
    (= (__ 1) 1)
    (= (__ 3) 6)
    (= (__ 5) 120)
    (= (__ 8) 40320))
  (letfn [(fact [n] (reduce * (range 1 (inc n))))]
    (and
     (= (fact 1) 1)
     (= (fact 3) 6)
     (= (fact 5) 120)
     (= (fact 8) 40320))))

(defn problem43 []
  (comment
    (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
    (= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
    (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))
  (letfn [(uninterleave [c n] (apply map list (partition n c)))]
    (and
     (= (uninterleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
     (= (uninterleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
     (= (uninterleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))))

(defn problem44 []
  (comment
    (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
    (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
    (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
    (= (__ 1 '(:a :b :c)) '(:b :c :a))
    (= (__ -4 '(:a :b :c)) '(:c :a :b)))
  (letfn [(rot-n [n c] ((fn [[a b]] (concat b a))
                        (split-at (mod n (count c)) c)))]
    (and
     (= (rot-n 2 [1 2 3 4 5]) '(3 4 5 1 2))
     (= (rot-n -2 [1 2 3 4 5]) '(4 5 1 2 3))
     (= (rot-n 6 [1 2 3 4 5]) '(2 3 4 5 1))
     (= (rot-n 1 '(:a :b :c)) '(:b :c :a))
     (= (rot-n -4 '(:a :b :c)) '(:c :a :b)))))

(defn problem45 []
  ;(= __ (take 5 (iterate #(+ 3 %) 1)))
  (= [1 4 7 10 13] (take 5 (iterate #(+ 3 %) 1))))

(defn problem46 []
  (comment
    (= 3 ((__ nth) 2 [1 2 3 4 5]))
    (= true ((__ >) 7 8))
    (= 4 ((__ quot) 2 8))
    (= [1 2 3] ((__ take) [1 2 3 4 5] 3)))
  (letfn [(rev-args [f] (fn [a b] (f b a)))]
    (and
     (= 3 ((rev-args nth) 2 [1 2 3 4 5]))
     (= true ((rev-args >) 7 8))
     (= 4 ((rev-args quot) 2 8))
     (= [1 2 3] ((rev-args take) [1 2 3 4 5] 3)))))

(defn problem47 []
  (comment
    (contains? #{4 5 6} __)
    (contains? [1 1 1 1 1] __)
    (contains? {4 :a 2 :b} __)
    (not (contains? '(1 2 4) __)))
  (let [x 4]
    (and
     (contains? #{4 5 6} x)
     (contains? [1 1 1 1 1] x)
     (contains? {4 :a 2 :b} x))))
     ;(not (contains? '(1 2 4) x)))))

(defn problem48 []
  (comment
    (= __ (some #{2 7 6} [5 6 7 8]))
    (= __ (some #(when (even? %) %) [5 6 7 8])))
  (let [x 6]
    (and
     (= x (some #{2 7 6} [5 6 7 8]))
     (= x (some #(when (even? %) %) [5 6 7 8])))))

(defn problem49 []
  (comment
    (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
    (= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
    (= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))
  (letfn [(my-split-at [n coll] [(take n coll) (drop n coll)])]
    (and
     (= (my-split-at 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
     (= (my-split-at 1 [:a :b :c :d]) [[:a] [:b :c :d]])
     (= (my-split-at 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))))

(defn problem50 []
  (comment
    (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
    (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
    (= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))
  (letfn [(split-by [c] (vals (group-by type c)))]
    (and
     (= (set (split-by [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
     (= (set (split-by [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
     (= (set (split-by [[1 2] :a [3 4] 5 6 :b]))
        #{[[1 2] [3 4]] [:a :b] [5 6]}))))

(defn problem51 []
  ;(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))
  (let [x (range 1 6)]
    (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] x] [a b c d]))))

(defn problem52 []
  ;(= [2 4] (let [[a b c d e f g] (range)] __))
  (= [2 4] (let [[a b c d e f g] (range)] [c e])))

;TODO:
(comment
(defn problem53 []
  (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
  (= (__ [5 6 1 3 2 7]) [5 6])
  (= (__ [2 3 3 4 5]) [3 4 5])
  (= (__ [7 6 5 4]) [])
))

;TODO:
(defn problem54 []
  (comment
    (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
    (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
    (= (__ 3 (range 8)) '((0 1 2) (3 4 5))))
)

(defn problem55 []
  (comment
    (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
    (= (__ [:b :a :b :a :b]) {:a 2, :b 3})
    (= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))
  (letfn [(my-frequencies [c] (into {}
                                    (for [[k v] (group-by identity c)]
                                      [k (count v)])))]
    (and
     (= (my-frequencies [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
     (= (my-frequencies [:b :a :b :a :b]) {:a 2, :b 3})
     (= (my-frequencies '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))))

(defn problem56 []
  (comment
    (= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
    (= (__ [:a :a :b :b :c :c]) [:a :b :c])
    (= (__ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
    (= (__ (range 50)) (range 50)))
  (letfn [(my-distinct [c] (reduce #(if (some (partial = %2) %1)
                                      %1
                                      (conj %1 %2)) [] c))]
    (and
     (= (my-distinct [1 2 1 3 1 2 4]) [1 2 3 4])
     (= (my-distinct [:a :a :b :b :c :c]) [:a :b :c])
     (= (my-distinct '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
     (= (my-distinct (range 50)) (range 50)))))

(defn problem57 []
  ;(= __ ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))
  (= [5 4 3 2 1] ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5)))

;TODO:
(defn problem58 []
  (comment
    (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
    (= 5 ((__ (partial + 3) second) [1 2 3 4]))
    (= true ((__ zero? #(mod % 8) +) 3 5 7 9))
    (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world")))
  (letfn [(fn-comp [& more]
            (fn [& x] (reverse more)))]
    (and
     (= [3 2 1] ((fn-comp rest reverse) [1 2 3 4]))
     (= 5 ((fn-comp (partial + 3) second) [1 2 3 4]))
     (= true ((fn-comp zero? #(mod % 8) +) 3 5 7 9))
     (= "HELLO" ((fn-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world")))))

(defn problem59 []
  (comment
    (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
    (= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello"))
    (= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))
  (letfn [(my-juxt [& more]
            (fn [& x] (reduce #(conj %1 (apply %2 x)) [] more)))]
    (and
     (= [21 6 1] ((my-juxt + max min) 2 3 5 1 6 4))
     (= ["HELLO" 5] ((my-juxt #(.toUpperCase %) count) "hello"))
     (= [2 6 4] ((my-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))))

;TODO:
(defn problem60 []
  (comment
    (= (take 5 (__ + (range))) [0 1 3 6 10])
    (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
    (= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))
  (letfn [(my-reductions [f & args])]
    (and
     (= (take 5 (my-reductions + (range))) [0 1 3 6 10])
     (= (my-reductions conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
     (= (last (my-reductions * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))))

(defn problem61 []
  (comment
    (= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
    (= (__ [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
    (= (__ [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))
  (letfn [(my-zipmap [x y] (into {} (map vector x y)))]
    (and
     (= (my-zipmap [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
     (= (my-zipmap [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
     (= (my-zipmap [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))))

(defn problem62 []
  (comment
    (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
    (= (take 100 (__ inc 0)) (take 100 (range)))
    (= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3]))))
  (letfn [(my-iterate [f x]
            (cons x (lazy-seq (my-iterate f (f x)))))]
    (= (take 5 (my-iterate #(* 2 %) 1)) [1 2 4 8 16])
    (= (take 100 (my-iterate inc 0)) (take 100 (range)))
    (= (take 9 (my-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

(defn problem63 []
  (comment
    (= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
    (= (__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
       {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
    (= (__ count [[1] [1 2] [3] [1 2 3] [2 3]])
       {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))
  (letfn [(my-group-by [f c]
            (reduce #(assoc %1 (f %2) (conj (into [] (%1 (f %2))) %2)) {} c))]
    (and
     (= (my-group-by #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
     (= (my-group-by #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
        {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
     (= (my-group-by count [[1] [1 2] [3] [1 2 3] [2 3]])
        {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))))

(defn problem64 []
  (comment
    (= 15 (reduce __ [1 2 3 4 5]))
    (=  0 (reduce __ []))
    (=  6 (reduce __ 1 [2 3])))
  (let [x +]
   (and
     (= 15 (reduce x [1 2 3 4 5]))
     (=  0 (reduce x []))
     (=  6 (reduce x 1 [2 3])))))

;TODO: skipped
(defn probelm65 [])

(defn problem66 []
  (comment
    (= (__ 2 4) 2)
    (= (__ 10 5) 5)
    (= (__ 5 7) 1)
    (= (__ 1023 858) 33))
  (letfn [(gcd [a b]
            (if (= b 0)
              a
              (gcd b (mod a b))))]
    (and
     (= (gcd 2 4) 2)
     (= (gcd 10 5) 5)
     (= (gcd 5 7) 1)
     (= (gcd 1023 858) 33))))

;TODO: skipped
(defn problem67 []
  (comment
    (= (__ 2) [2 3])
    (= (__ 5) [2 3 5 7 11])
    (= (last (__ 100)) 541))
)

(defn problem68 []
  (comment
    (= __
       (loop [x 5
              result []]
         (if (> x 0)
           (recur (dec x) (conj result (+ 2 x)))
           result))))
  (= [7 6 5 4 3]
     (loop [x 5
            result []]
       (if (> x 0)
         (recur (dec x) (conj result (+ 2 x)))
         result))))

(defn problem69 []
  (comment
    (= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
       {:a 4, :b 6, :c 20})
    (= (__ - {1 10, 2 20} {1 3, 2 10, 3 15})
       {1 7, 2 10, 3 15})
    (= (__ concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
       {:a [3 4 5], :b [6 7], :c [8 9]}))
  (letfn [(my-merge-with [])]
    (and
     (= (my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
        {:a 4, :b 6, :c 20})
     (= (my-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
        {1 7, 2 10, 3 15})
     (= (my-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
        {:a [3 4 5], :b [6 7], :c [8 9]}))))

(defn problem70 []
  (comment
    (= (__  "Have a nice day.")
       ["a" "day" "Have" "nice"])
    (= (__  "Clojure is a fun language!")
       ["a" "Clojure" "fun" "is" "language"])
    (= (__  "Fools fall for foolish follies.")
       ["fall" "follies" "foolish" "Fools" "for"]))
  (letfn [(split-sort [s]
            (sort-by #(.toLowerCase %)
                     (re-seq #"[A-Za-z]+" s)))]
    (and
     (= (split-sort  "Have a nice day.")
        ["a" "day" "Have" "nice"])
     (= (split-sort  "Clojure is a fun language!")
        ["a" "Clojure" "fun" "is" "language"])
     (= (split-sort  "Fools fall for foolish follies.")
        ["fall" "follies" "foolish" "Fools" "for"]))))

(defn problem71 []
  (comment
    (= (__ (sort (rest (reverse [2 5 4 1 3 6]))))
       (-> [2 5 4 1 3 6] reverse rest sort __)
       5))
  (= (last (sort (rest (reverse [2 5 4 1 3 6]))))
     (-> [2 5 4 1 3 6] reverse rest sort last)
     5))

(defn problem72 []
  (comment
    (= (__ (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
       (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (__))
       11))
  (= (reduce +  (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
     (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce +))
     11))

;TODO: skipped
(defn problem73 [])

(defn problem74 []
  (comment
    (= (__ "4,5,6,7,8,9") "4,9")
    (= (__ "15,16,25,36,37") "16,25,36"))
  (letfn [(list-ps [s]
            (apply str
                   (interpose ","
                              (filter #(== (Math/sqrt %) (int (Math/sqrt %)))
                                      (map #(Integer/parseInt %)
                                           (re-seq #"[\d]+" s))))))]
    (and
     (= (list-ps "4,5,6,7,8,9") "4,9")
     (= (list-ps "15,16,25,36,37") "16,25,36"))))

(defn problem75 []
  (comment
    (= (__ 1) 1)
    (= (__ 10) (count '(1 3 7 9)) 4)
    (= (__ 40) 16)
    (= (__ 99) 60))
  (letfn [(euler-totient [])]
    (and
     (= (euler-totient 1) 1)
     (= (euler-totient 10) (count '(1 3 7 9)) 4)
     (= (euler-totient 40) 16)
     (= (euler-totient 99) 60))))

;TODO: skipped
(defn problem76 []
  (comment
    (= __
       (letfn
           [(foo [x y] #(bar (conj x y) y))
            (bar [x y] (if (> (last x) 10)
                         x
                         #(foo x (+ 2 y))))]
         (trampoline foo [] 1))))
  (= [1 3 5 7 9 11]
     (letfn
         [(foo [x y] #(bar (conj x y) y))
          (bar [x y] (if (> (last x) 10)
                       x
                       #(foo x (+ 2 y))))]
       (trampoline foo [] 1))))

;TODO: skipped
(defn problem77 [])

;TODO: skipped
(defn problem78 [])

(defn problem79 [])

(defn problem80 []
  (comment
    (= (__ 6) true)
    (= (__ 7) false)
    (= (__ 496) true)
    (= (__ 500) false)
    (= (__ 8128) true))
  (letfn [(is-perfect? [n]
            (= n (apply + (filter #(zero? (mod n %)) (range 1 n)))))]
    (and
     (= (is-perfect? 6) true)
     (= (is-perfect? 7) false)
     (= (is-perfect? 496) true)
     (= (is-perfect? 500) false)
     (= (is-perfect? 8128) true)))())

(defn problem81 []
  (comment
    (= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
    (= (__ #{0 1 2} #{3 4 5}) #{})
    (= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))
  (letfn [(my-intersection [a b]
            (into #{} (filter #(contains? b %) a)))]
    (and
     (= (my-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3})
     (= (my-intersection #{0 1 2} #{3 4 5}) #{})
     (= (my-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))))

;TODO: skipped
(defn problem82 [])

(defn problem83 []
  (comment
    (= false (__ false false))
    (= true (__ true false))
    (= false (__ true))
    (= true (__ false true false))
    (= false (__ true true true))
    (= true (__ true true true false)))
  (letfn [(half-truth [& more]
            (boolean (and (some true? more) (not-every? true? more))))]
    (= false (half-truth false false))
    (= true (half-truth true false))
    (= false (half-truth true))
    (= true (half-truth false true false))
    (= false (half-truth true true true))
    (= true (half-truth true true true false))))

;TODO: skipped
(defn problem84 [])

;TODO: write a function to test whether all problems pass or fail
(defn test-problems [x])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "running 4clojure tests")
  (test-problems [
                  problem1
                  problem2
                  problem3
                  problem4
                  problem5
                  problem6
                  problem7
                  problem8
                  problem9
                  problem10
                  problem11
                  problem12
                  problem13
                  problem14
                  problem15
                  problem16
                  problem17
                  problem18
                  problem19
                  problem20
                  problem21
                  ]))
