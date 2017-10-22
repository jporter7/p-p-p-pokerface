(ns p-p-p-pokerface)

(defn rank [card]
  (let [val (get card 0)]
     (if (Character/isDigit val)
       (Integer/valueOf (str val))
       (get {\T 10 \J 11 \Q 12 \K 13 \A 14} val))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (let [ranks (map rank hand)
         freq (frequencies ranks)
         val (vals freq)
         check (fn [x] (== x 2))]
     (not (empty? (filter check val)))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
         freq (frequencies ranks)
         val (vals freq)
         check (fn [x] (== x 3))]
     (not (empty? (filter check val)))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
         freq (frequencies ranks)
         val (vals freq)
         check (fn [x] (== x 4))]
     (not (empty? (filter check val)))))

(defn flush? [hand]
  (let [suits (map suit hand)]
     (apply = suits)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) (= (count (filter even? (vals (frequencies (map rank hand))))) 2)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
         rankslow (sort (replace {14 1} ranks))
         low (apply min ranks)
         high (apply max ranks)
         str (range low (+ low 5))
         str2 (range 1 6)]
     (or (= ranks str) (= rankslow str2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[pair? 1]
                  [two-pairs? 2]  [three-of-a-kind? 3]
                  [straight? 4]   [flush? 5]
                  [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]}]
   (apply max (map (fn [x] (if ((first x) hand) (second x) 0)) checkers))))
