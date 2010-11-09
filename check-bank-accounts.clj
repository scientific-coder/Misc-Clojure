;; implements spanish bank account validation
(defn- to-digits [s] (map #(Character/getNumericValue %) s))

(defn valid-spanish? [s]
  (let [chk-digit (fn [code] (let [tmp (->> code (map * [1 2 4 8 5 10 9 7 3 6])
					    (reduce +) mod (- 11))]
			       (case tmp
				     11 0
				     10 1
				     tmp)))
	
	bank-branch (->> digits (take 8) (into [0 0]))
	account (drop 10 digits)]
    (and (= (chk-digit bank-branch) (nth digits 8))
	 (= (chk-digit account) (nth digits 9)))))
;;tests
(valid-spanish? "21000418450200051332")
(valid-spanish? "21000418450200051333")

(defn valid-finnish? [acc]
  (let[mod10 #(mod % 10)]
    (zero? (mod10 (apply +(to-digits (apply str (map * [2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1] (to-digits acc)))))))))

(valid-finnish? "42345670000081"); true
(valid-finnish? "12345600000785"); true
(valid-finnish? "12345600000782"); false
(valid-finnish? "12345600000718"); false
