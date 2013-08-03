(def mp {:a 1 :b 2 :c 3})
(def mp (hash-map :a 1 :b 2 :c 3))

(:a mp) ; 1
(mp :a) ; 1

(assoc mp :d 4) ; {:a 1 :b 2 :c 3 :d 4}
(dissoc mp :a) ; {:b 2 :c 3}