(ns hpointu.protocol)

(defn payload-concat [payloads]
  (apply str payloads))

(defn- recombine-payload [segments]
  (->> segments
       (sort-by ::index)
       (map ::payload)
       (payload-concat)))

(defn recompose []
  (fn [xf]
    (let [reg (atom {})]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (let [id (::id input)]
           (swap! reg update id conj input)
           (if (= (count (into #{} (map ::index (@reg id)))) (::total input))
             (let [recombined-payload (recombine-payload (@reg id))]
               (swap! reg dissoc id)
               (xf result {::index id ::payload recombined-payload}))
             result)))))))


(def packets
  [{::id 123 ::index 2 ::total 2 ::payload " toi"}
   {::id 44 ::index 1 ::total 1 ::payload "Hello"}
   {::id 123 ::index 1 ::total 2 ::payload "Coucou"}])

(sequence (recompose) packets)
