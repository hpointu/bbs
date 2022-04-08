(ns hpointu.protocol
  (:require [clojure.edn :as edn]))

(defn payload-concat [payloads]
  (apply str payloads))

(defn- recombine-payload [segments]
  (->> segments
       (sort-by ::index)
       (map ::payload)
       (payload-concat)
       (edn/read-string)))

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

(defn chop [packet size]
  (let [chopped (partition-all size (pr-str (::payload packet)))]
    (for [[i c] (map-indexed vector chopped)]
      {::id (::id packet)
       ::payload (apply str c)
       ::index i
       ::total (count chopped)})))

(def packets
  (-> (concat (chop {::id 123 ::payload "Coucou les amis"} 6)
              (chop {::id 456 ::payload "Je suis un paquet perdu"} 8)
              (chop {::id 789 ::payload {:some "payload" :with {:nested :thing}}} 8))
      shuffle))

(sequence (recompose) packets)
